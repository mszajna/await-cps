(ns await-cps
  (:refer-clojure :exclude [await deref])
  (:require [clojure.walk :refer [macroexpand-all]]))

(defn await [f & args]
  (throw (new IllegalStateException "await called outside async block")))

(def ^:private ^:dynamic recur-target) ; compile-time use only

(defn- walk [form r e]
  (let [call (fn [] (walk (first form) (fn [x] (if (seq (rest form)) (walk (rest form) (fn [xs] (r `(~x ~@xs))) e) (r `(~x)))) e))]
    (cond
      (and (seq? form) (special-symbol? (first form)))
      (let [[head & tail] form]
        (case head
          (quote var fn* def deftype* reify letfn* import) (r form)
          (. new set! monitor-enter monitor-exit) (call)
          do (let [[x & xs] tail]
              (cond
                (empty? xs) (walk x r e)
                :else (walk x (fn [x] (walk `(do ~@xs) (fn [xs] (r `(do ~x ~xs))) e)) e)))
          if (walk (first tail) (fn [c] `(if ~c ~@(map #(walk % r e) (rest tail)))) e)
          case* (let [[ge shift mask default imap & args] tail]
                  `(case* ~ge ~shift ~mask ~(walk default r e) ~(reduce-kv #(assoc %1 %2 (update %3 1 (fn [v] (walk v r e)))) {} imap) ~@args))
          let* (let [[[sym value & binds] & body] tail]
                (walk value (fn [v] `(let [~sym ~v] ~(walk (if binds `(let* [~@binds] ~@body) `(do ~@body)) r e))) e))
          try (let [[body catches] (split-with #(not (and (seq? %) (#{'catch 'finally} (first %)))) tail)
                    catches (map (fn [[kind cls bnd & catch-body ]]
                                   (when (= 'finally kind) (throw (new UnsupportedOperationException "finally blocks are not supported in async macro")))
                                   `(~kind ~cls ~bnd ~(walk `(do ~@catch-body) r e))) catches)]
                `(try ~(walk `(do ~@body) r (fn [t] (let [ex (gensym "ex")] `(try (throw ~t) ~@catches (catch Throwable ~ex ~(e ex))))))
                  ~@catches))
          throw (walk (first tail) e e) ; TODO: don't silently ignore extra params, blow up!
          loop* (let [[binds & body] tail
                      bind-names (->> binds (partition 2) (map first))]
                  (walk `(let* [~@binds] ~bind-names)
                        (fn [bind-vals]
                          (binding [recur-target (gensym "rec")]
                            (clojure.walk/prewalk identity ; `binding` doesn't play nice with lazy seq
                              `(letfn [(~recur-target [~@bind-names] ~(walk `(do ~@body) r e))] (~recur-target ~@bind-vals))))) e))
          recur (walk tail (fn [step-vals] `(~recur-target ~@step-vals)) e)
          (throw (ex-info "Unknown special form" {:unknown-special-form head}))))

      (and (seq? form) (symbol? (first form)) (= #'await (resolve (first form))))
      (let [v (gensym "v") t (gensym "t")]
        (walk (rest form) (fn [call-form] `(~@call-form (fn [~v] ~(r v)) (fn [~t] ~(e t)))) e))
      
      (seq? form) (call)
      (vector? form) (walk (seq form) (comp r vec) e)
      (map? form) (walk (mapcat identity form) #(->> % (partition 2) (map vec) (into {}) r) e)
      (set? form) (walk (seq form) (comp r set) e)
      :else (r form))))

(defn async* [ret err form]
  (-> form macroexpand-all (walk (fn [v] `(~ret ~v)) (fn [t] `(~err ~t)))))

(defmacro async [ret err & body]
  (async* ret err `(do ~@body)))
