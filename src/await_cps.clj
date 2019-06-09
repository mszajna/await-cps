(ns await-cps
  (:refer-clojure :exclude [await deref])
  (:require [clojure.walk :refer [macroexpand-all]]))

(defn await [f & args]
  (throw (new IllegalStateException "await called outside async block")))

(def ^:private ^:dynamic recur-target) ; compile-time use only

(defn- walk [form r e]
  (let [call (fn [] (walk (vec form) (fn [args] (let [v (gensym "v")] `(let [~v (~@args)] ~(r v)))) e))]
    (cond
      (and (seq? form) (special-symbol? (first form)))
      (let [[head & tail] form]
        (case head
          (quote var fn* def deftype* reify letfn* import) (r form)
          (. new set! monitor-enter monitor-exit) (call)
          do (let [[x & xs] tail]
               (if (empty? xs)
                 (walk x r e)
                 (walk x (fn [x] (walk `(do ~@xs) (fn [xs] (r `(do ~x ~xs))) e)) e)))
          if (walk (first tail) (fn [c] `(if ~c ~@(map #(walk % r e) (rest tail)))) e)
          case* (let [[ge shift mask default imap & args] tail]
                  `(case* ~ge ~shift ~mask ~(walk default r e) ~(reduce-kv #(assoc %1 %2 (update %3 1 (fn [v] (walk v r e)))) {} imap) ~@args))
          let* (let [[[sym value & binds] & body] tail]
                (walk value (fn [v] `(let [~sym ~v] ~(walk (if binds `(let* [~@binds] ~@body) `(do ~@body)) r e))) e))
          try (let [[body catches] (split-with #(not (and (seq? %) (#{'catch 'finally} (first %)))) tail)
                    [catches finally] (if (->> catches last first (= 'finally)) [(drop-last catches) (rest (last catches))] [catches])
                    catch-forms (map (fn [[c cls bnd & body]] `(~c ~cls ~bnd ~(walk `(do ~@body) (if-not finally r (fn [cbody] (walk `(do ~@finally) (fn [fin] `(do ~fin ~(r cbody))) e))) e))) catches)]
                `(try ~(walk `(do ~@body)
                             (fn [v] (if-not finally (r v) (walk `(do ~@finally) (fn [fin] `(do ~fin ~(r v))) e)))
                             (fn [t] (let [ex (gensym "ex")] `(try (throw ~t) ~@catch-forms
                                                                   (catch Throwable ~ex ~(if-not finally (e ex) (walk `(do ~@finally) (fn [fin] `(do ~fin ~(e ex))) e)))))))
                  ~@catch-forms
                  ~(let [t (gensym "t")] `(catch Throwable ~t ~(if-not finally `(throw ~t) (walk `(do ~@finally) (fn [fin] `(do ~fin ~(e t))) e))))))
          throw (walk (first tail) e e) ; TODO: don't silently ignore extra params, blow up!
          loop* (let [[binds & body] tail
                      bind-names (->> binds (partition 2) (map first))]
                  (walk `(let* [~@binds] [~@bind-names])
                        (fn [bind-vals]
                          (binding [recur-target (gensym "rec")]
                            (clojure.walk/prewalk identity ; `binding` doesn't play nice with lazy seq
                              `(letfn [(~recur-target [~@bind-names] ~(walk `(do ~@body) r e))] (~recur-target ~@bind-vals))))) e))
          recur (walk (vec tail) (fn [step-vals] `(~recur-target ~@step-vals)) e)
          (throw (ex-info "Unknown special form" {:unknown-special-form head}))))

      (and (seq? form) (symbol? (first form)) (= #'await (resolve (first form))))
      (let [v (gensym "v") t (gensym "t")]
        (walk `(partial ~@(rest form)) (fn [f] `(~f (fn [~v] ~(r v)) (fn [~t] ~(e t)))) e))

      (seq? form) (call)
      (vector? form) ((reduce (fn [r x] (fn [xs] (walk x (fn [x'] (r `(~@xs ~x'))) e)))
                              (fn [xs] (r (vec xs)))
                              (reverse form)) nil)
      (map? form) (walk (vec (mapcat identity form)) #(->> % (partition 2) (map vec) (into {}) r) e)
      (set? form) (walk (vec form) (comp r set) e)
      :else (r form))))

(defn async* [ret err form]
  (-> form macroexpand-all (walk (fn [v] `(~ret ~v)) (fn [t] `(~err ~t)))))

(defmacro async [ret err & body]
  (let [r (gensym "r")
        e (gensym "e")]
    `(let [~r ~ret ~e ~err] ~(async* r e `(do ~@body)))))
