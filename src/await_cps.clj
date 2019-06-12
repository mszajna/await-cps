(ns await-cps
  (:refer-clojure :exclude [await deref])
  (:require [clojure.walk :refer [prewalk macroexpand-all]]))

(defn await [f & args]
  (throw (new IllegalStateException "await called outside async block")))

(defn- has-async? [form]
  (or (and (coll? form) (some has-async? form))
      (and (seq? form) (symbol? (first form)) (= #'await (resolve (first form))))))

(defn- ->do [forms]
  (if (<= 2 (count forms)) `(do ~@forms) (first forms)))

(def ^:private ^:dynamic recur-target) ; compile-time use only
(def ^:private ^:dynamic top-err-handler)

; (try (await a) (catch T t b))
; (try (a (fn [a'] (try (r a') (catch Thr t (e t)))) (fn [t] (try (throw t) (catch T t (r b))))) (catch Thr t (e t)))

; (try (await a) b (catch T t c))
; (try (a (fn [a'] (try (try-let [v b] (catch T t (r c)) (r v)) (catch Thr t (e t)))) ...

; (a (try (await b) (catch T t c))) | v => (r v), e => e
; (try (await b) (catch T t c)) | v => (r (a v)), e => e
; (await b) | v => (r (a v)) | e => (try e (catch T t (r (a c))))
; => (b (fn [v] (r (a v))) (fn [e] (try (throw e) (catch T t (r (a c))))))

; (walk (a (try (await b) (c) (await d) (catch T t e))), v => (r v), v => v)
; (walk (try (await b) (c) (await d) (catch T t e)), v => (r (a v)), v => v)
; (let [ctch (fn [v] (try v (catch T t (r (a e)))))])
; (ctch (walk (do (await b) (c) (await d)), v => (r (a v)), ctch))
; (ctch (walk (await b), v => (ctch (walk (do (c) (await d)), v => (r (a v)), ctch)), ctch))
; (ctch (b (fn [b'] (ctch (walk (do (c) (await d)), v => (r (a v)), ctch)) (fn [t] (ctch (throw t))))))
; (ctch (b (fn [b'] (ctch (do (c) (walk (await d), v => (r (a v)), ctch)))) (fn [t] (ctch (throw t)))))
; (ctch (b (fn [b'] (ctch (do (c) (d (fn [d'] (r (a d'))) (fn [t] (ctch (throw t))))))) (ctch (throw t))))
; ====

; (walk (a (try (await b) (c) (await d) (catch T t e))), v => (r v))
; (walk (try (await b) (c) (await d) (catch T t e)), v => (r (a v)))
; (walk (try (b (fn [b'] (walk (try (c) (await d) (catch T t (r (a e)))), v => (r (a v))))) (catch T t (r (a e)))))

; (walk (a (try (b (await c) (await d)) (catch T t e))), v => (r v))
; (walk (try (b (await c) (await d)) (catch T t e)), v => (r (a v)))
; (walk (b (await c) (await d)), v => (try-let [x v] (r (a v)) (catch T t (r (a e)))), v => (try v (catch T t (r (a e)))))
;;; (let [ctch (fn [r v] `(try-let [x# ~v] (catch T t ~(r e)) ~(r x))])
; (try (c (fn [c'] (walk (try (b c' (await d)) (catch T t e)), v => (r (a v))))
;         (fn [t] (try (throw t) (catch T t (r (a e))))))
;   (catch T t (r (a e))))
; (try (c (fn [c'] (try (d (fn [d'] (try-let [v (b c' d')] (r (a v)) (catch T t (r (a e)))))
;                          (fn [t] (try (throw t) (catch T t (r (a e))))))
;                    (catch T t (r (a e)))))
;         (fn [t] (try (throw t) (catch T t (r (a e))))))
;  (catch T t (r (a e))))

; (walk (f (try+ (if (a) (c (await b)) (e)))) v=>(r v), v=>(try v (catch * t (e t)))
; (walk (try+ (if (a) (c (await b)) (e))) v=>(r (f v)), v=>(try v (catch * t (e t)))
; (try+ (walk (if (a) (c (await b)) (e)) v=>(let [x v] (future (try (r (f x)) (catch * t (e t))))), v=>(try (try+ v) (catch * t (e t)))
; (try+ (if (a) (walk (c (await b)) ..)
            ;   (walk (e) ..)))
; (try+ (if (a) (walk (await b) v=>(let [x (c v)] (future (try (r (f x)) (catch * t (e t))))), v=>(try (try+ v) (catch * t (e t))))
             ;  (let [x (e)] (future (try (r (f x)) (catch * t (e t)))))))
; (try+ (if (a) (b (fn [b'] (try (try+ (let [x (c b')] (future (try (r (f x)) (catch * t (e t)))))) (catch * t (e t)))) ..)
             ;  (let [x (e)] (future (try (r (f x)) (catch * t (e t)))))))

; (walk (try+ (a (if b (await c) (d)))) r: v=>(respond v) e: v=>(try v (catch * t (raise t))))
; (letfn [(unthrow [v] (future (e (r v))))])
; (try+ (walk (a (if b (await c) (d)))) r: v=>(unthrow v) e: v=>(e (try+ v)))
; (try+ (walk (if b (await c) (d)) r: v=>(r (a v)), e: e))
; (try+ (if b (walk (await c) r: r, e: e) (walk (d) r: r, e: e)))


; `r` is a function given a walked `form` value returns that form with continuation
(defn- walk [form r e]
  (let [form (macroexpand form)
        call (fn [] (walk (vec form) (comp r seq) e))]
    (cond
      (not (has-async? form)) (r form)
      (and (seq? form) (special-symbol? (first form)))
      (let [[head & tail] form]
        (case head
          (quote var fn* def deftype* reify letfn* import) (r form)
          (. new set! monitor-enter monitor-exit) (call)
          do (let [[syncs [asn & others]] (split-with #(not (has-async? %)) tail)
                   asn-form (when asn (walk asn (fn [v] (if others (walk (->do others) r e) (r v))) e))]
               (cond (and (seq syncs) asn-form) `(do ~@syncs ~asn-form)
                     asn-form asn-form
                     :else (r `(do ~@syncs))))
          if (walk (first tail) (fn [c] `(if ~c ~@(map #(walk % r e) [(second tail) (nth tail 2 nil)]))) e)
          case* (let [[ge shift mask default imap & args] tail]
                  `(case* ~ge ~shift ~mask ~(walk default r e) ~(reduce-kv #(assoc %1 %2 (update %3 1 (fn [v] (walk v r e)))) {} imap) ~@args))
          let* (let [[syncs [[sym asn] & others]] (->> tail first (partition 2) (split-with #(not (has-async? %))))
                     asn-form (when asn (walk asn (fn [v] `(let [~sym ~v] ~(if others (walk `(let* [~@(mapcat identity others)] ~@(rest tail)) r e) (walk (->do (rest tail)) r e)))) e))]
                 (cond (and (seq syncs) asn-form) `(let* [~@(mapcat identity syncs)] ~asn-form)
                       asn-form asn-form
                       :else `(let* [~@(mapcat identity syncs)] ~(walk (->do (rest tail)) r e))))
          ; try (let [[body cfs] (split-with #(not (and (seq? %) (#{'catch 'finally} (first %)))) tail)
          ;           [catches finally] (if (->> cfs last first (= 'finally)) [(drop-last cfs) (rest (last cfs))] [cfs])
          ;           cf-forms `[~@(map (fn [[c cls bnd & body]] `(~c ~cls ~bnd ;~(walk (if-not finally (->do body) `(let [x# ~(->do body)] ~@finally x#)) r e))) catches)
          ;                                                                     ~(if-not finally (walk (->do body) r e)
          ;                                                                             (walk (->do body) (fn [v] `(try ~v (finally ~@finally))) (fn [t] `(try ~t (finally ~@finally))))))) catches)
          ;                     ~@(when finally [(let [t (gensym "t")] `(catch Throwable ~t ~(walk (->do finally) (fn [fin] `(do ~fin (throw ~t))) e)))])]]
          ;       `(try ~(walk (->do body)
          ;                   (fn [v] (if-not finally (r v) (walk `(let [x# ~v] ~@finally x#) r e)))
          ;                   (fn [t] (let [ex (gensym "ex")] `(try (try (throw ~t) ~@cf-forms) (catch Throwable ~ex ~(e ex))))))
          ;         ~@cf-forms))
          ; try (let [[body cfs] (split-with #(not (and (seq? %) (#{'catch 'finally} (first %)))) tail)
          ;           [catches finally] (if (->> cfs last first (= 'finally)) [(map rest (drop-last cfs)) (rest (last cfs))] [(map rest cfs)])
          ;           fin (gensym "fin") fin-thr (gensym "fin-thr") cat (gensym "cat") v (gensym "v")]
          ;       `(letfn [(~fin [~v] ~(walk (->do finally) (fn [f] `(do ~f ~(r v))) e))
          ;               (~fin-thr [~v] ~(walk (->do finally) (fn [f] `(do ~f ~(e v))) e))
          ;               (~cat [t#] (try (try (throw t#)
          ;                                 ~@(map (fn [[cls bnd & body]]
          ;                                         `(catch ~cls ~bnd ~(walk (->do body) (fn [v] `(let [[success# x#] (try [true ~v] (catch Throwable t# (~fin-thr t#) [false]))] (when success# (~fin x#)))) (fn [t] `(~fin-thr ~t))))) catches))
          ;                             (catch Throwable t'# (~fin-thr t'#))))]
          ;         ~(if-not (has-async? body) `(let [[success# x#] (try [true ~(->do body)] (catch Throwable t# (~cat t#) [false]))] (when success# (~fin x#)))
          ;                 `(try ~(walk (->do body) (fn [v] `(let [[success# x#] (try [true ~v] (catch Throwable t# (~cat t#) [false]))] (when success# (~fin x#)))) (fn [t] `(~cat ~t)))
          ;                     (catch Throwable t# (~cat t#))))))
          try (let [[body cfs] (split-with #(not (and (seq? %) (#{'catch 'finally} (first %)))) tail)
                    [catches finally] (if (->> cfs last first (= 'finally)) [(map rest (drop-last cfs)) (rest (last cfs))] [(map rest cfs)])
                    untry (gensym "untry") v (gensym "v") untry-throw (gensym "untry-throw")
                    wrap-finally (fn [form] `(try ~form (catch Throwable t# (~untry-throw t#))))
                    wrap-catches (fn [form] (wrap-finally `(try ~form
                                              ~@(map (fn [[cls bnd & body]] `(catch ~cls ~bnd ~(walk (->do body) (fn [v] `(~untry ~v)) (fn [form] (e (wrap-finally form)))))) catches))))]
                `(letfn [(~untry [~v] (future ~(e (walk (->do finally) (fn [fin] `(do ~fin ~(r v))) e))))
                         (~untry-throw [~v] (future ~(e (walk (->do finally) (fn [fin] `(do ~fin (throw ~v))) e))))]
                  ~(wrap-catches (walk (->do body) (fn [v] `(~untry ~v)) (fn [v] (e (wrap-catches v)))))))
          loop* (let [[binds & body] tail
                      bind-names (->> binds (partition 2) (map first))]
                  (walk `(let [~@binds] [~@bind-names])
                        (fn [bind-vals]
                          (binding [recur-target (gensym "rec")]
                            (prewalk identity ; `binding` doesn't play nice with lazy seq
                              `(letfn [(~recur-target [~@bind-names] ~(walk (->do body) r e))] (apply ~recur-target ~bind-vals))))) e))
          recur (walk (vec tail) (fn [step-vals] `(~recur-target ~@step-vals)) e)
          (throw (ex-info "Unknown special form" {:unknown-special-form head}))))

      (and (seq? form) (symbol? (first form)) (= #'await (resolve (first form))))
      (let [v (gensym "v") t (gensym "t")]
        (walk `[~@(rest form)] (fn [f] `(~@f (fn [~v] ~(e (r v))) ; (catch Throwable ~t ~(top-err-handler t))))
                                             (fn [~t] ~(e `(throw ~t))))) e)) ; (catch Throwable ~t ~(top-err-handler t)))))) e))

      (seq? form) (call)
      (vector? form) ((reduce (fn [r x] (fn [xs] (walk x (fn [x'] (if (coll? x')
                                                                    (let [v (gensym "v")] `(let [~v ~x] ~(r `(~@xs ~v))))
                                                                    (r `(~@xs ~x')))) e)))
                              (fn [xs] (r (vec xs)))
                              (reverse form)) nil)
      (map? form) (walk (vec (mapcat identity form)) #(->> % (partition 2) (map vec) (into {}) r) e)
      (set? form) (walk (vec form) (comp r set) e)
      :else (r form))))

(defn async* [ret err form]
  ;(binding [top-err-handler (fn [t] `(~err ~t))]
  (let [t (gensym "t")]
    (prewalk identity
      (walk form (fn [v] `(~ret ~v)) (fn [form] `(try ~form (catch Throwable ~t (~err ~t))))))))

(defmacro async [ret err & body]
  (let [r (gensym "r")
        e (gensym "e")]
    `(let [~r ~ret ~e ~err] ~(async* r e `(do ~@body)))))
