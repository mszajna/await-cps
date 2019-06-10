(ns await-cps
  (:refer-clojure :exclude [await deref])
  (:require [clojure.walk :refer [macroexpand-all]]))

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
          if (walk (first tail) (fn [c] `(if ~c ~@(map #(walk % r e) (rest tail)))) e)
          case* (let [[ge shift mask default imap & args] tail]
                  `(case* ~ge ~shift ~mask ~(walk default r e) ~(reduce-kv #(assoc %1 %2 (update %3 1 (fn [v] (walk v r e)))) {} imap) ~@args))
          let* (let [[syncs [[sym asn] & others]] (->> tail first (partition 2) (split-with #(not (has-async? %))))
                     asn-form (when asn (walk asn (fn [v] `(let [~sym ~v] ~(if others (walk `(let* [~@(mapcat identity others)] ~@(rest tail)) r e) (walk (->do (rest tail)) r e)))) e))]
                 (cond (and (seq syncs) asn-form) `(let* [~@(mapcat identity syncs)] ~asn-form)
                       asn-form asn-form
                       :else `(let* [~@(mapcat identity syncs)] ~(walk (->do (rest tail)) r e))))
          try (let [[body cfs] (split-with #(not (and (seq? %) (#{'catch 'finally} (first %)))) tail)
                    [catches finally] (if (->> cfs last first (= 'finally)) [(drop-last cfs) (rest (last cfs))] [cfs])
                    cf-forms `[~@(map (fn [[c cls bnd & body]] `(~c ~cls ~bnd ~(walk (if-not finally (->do body) `(let [x# ~(->do body)] ~@finally x#)) r e))) catches)
                               ~@(when finally [(let [t (gensym "t")] `(catch Throwable ~t ~(walk (->do finally) (fn [fin] `(do ~fin (throw ~t))) e)))])]]
                `(try ~(walk (->do body)
                             (fn [v] (if-not finally (r v) (walk `(let [x# ~v] ~@finally x#) r e)))
                             (fn [t] (let [ex (gensym "ex")] `(try (try (throw ~t) ~@cf-forms) (catch Throwable ~ex ~(e ex))))))
                  ~@cf-forms))
          loop* (let [[binds & body] tail
                      bind-names (->> binds (partition 2) (map first))]
                  (walk `(let [~@binds] [~@bind-names])
                        (fn [bind-vals]
                          (binding [recur-target (gensym "rec")]
                            (clojure.walk/prewalk identity ; `binding` doesn't play nice with lazy seq
                              `(letfn [(~recur-target [~@bind-names] ~(walk (->do body) r e))] (apply ~recur-target ~bind-vals))))) e))
          recur (walk (vec tail) (fn [step-vals] `(~recur-target ~@step-vals)) e)
          (throw (ex-info "Unknown special form" {:unknown-special-form head}))))

      (and (seq? form) (symbol? (first form)) (= #'await (resolve (first form))))
      (let [v (gensym "v") t (gensym "t")]
        (walk `[~@(rest form)] (fn [f] `(~@f (fn [~v] (try ~(r v) (catch Throwable ~t ~(e t))))
                                             (fn [~t] ~(e t)))) e))

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
  (walk form (fn [v] `(~ret ~v)) (fn [t] `(~err ~t))))

(defmacro async [ret err & body]
  (let [r (gensym "r")
        e (gensym "e")]
    `(let [~r ~ret ~e ~err] ~(async* r e `(do ~@body)))))
