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
        (walk `[~@(rest form)] (fn [f] `(~@f (fn [~v] ~(e (r v)))
                                             (fn [~t] ~(e `(throw ~t))))) e))

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
  (let [t (gensym "t")]
    (walk form (fn [v] `(~ret ~v)) (fn [form] `(try ~form (catch Throwable ~t (~err ~t)))))))

(defmacro async [ret err & body]
  (let [r (gensym "r")
        e (gensym "e")]
    `(let [~r ~ret ~e ~err] ~(async* r e `(do ~@body)))))
