(ns await.generators
  (:require [clojure.test.check.generators :as gen]))

(defn ->bool [v] (zero? (mod (hash v) 2)))

(defn an-if [body-gen]
  (gen/fmap (fn [[c & alts]] `(if (->bool ~c) ~@alts))
            (gen/vector body-gen 2 3)))

(defn a-case [body-gen]
  (gen/fmap (fn [[exp value cases default]] `(case (do ~exp ~value) ~@(mapcat identity cases) ~default))
            (gen/tuple body-gen
                       (gen/elements [:a :b])
                       (gen/vector-distinct-by first (gen/tuple (gen/elements [:a :b :c]) body-gen) {:max-elements 3})
                       body-gen)))

(defn a-do [body-gen]
  (gen/fmap (fn [f] `(do ~@f))
            (gen/vector body-gen 0 3)))

(def symbols ['a 'b 'c])

(def a-symbol (gen/elements symbols))

(defn a-let [body-gen]
  (gen/fmap (fn [[bindings body]] `(let [~@(mapcat identity bindings)] ~@body))
            (gen/tuple (gen/vector (gen/tuple a-symbol body-gen) 1 2) (gen/vector body-gen 0 3))))

(defn a-loop [body-gen]
  (gen/fmap (fn [[n bindings body result]] `(loop [n# ~n ~@(mapcat identity bindings)]
                                              ~@body (if (> n# 0) (recur (dec n#) ~@bindings) ~result)))
            (gen/tuple (gen/choose 0 2) (gen/vector (gen/tuple a-symbol body-gen) 0 2) (gen/vector body-gen 0 2) body-gen)))

(defn a-catch [body-gen]
  (gen/fmap (fn [[cls body]] `(catch ~cls sym# ~@body))
            (gen/tuple (gen/elements ['clojure.lang.ExceptionInfo 'UnsupportedOperationException 'Throwable])
                       (gen/vector body-gen 0 2))))

(defn a-try [body-gen]
  (gen/fmap (fn [[body catches finally]] `(try ~@body ~@catches ~@(when (seq finally) `[(finally ~@finally)])))
            (gen/tuple (gen/vector body-gen 0 2) (gen/vector (a-catch body-gen) 0 2) (gen/vector body-gen 0 2))))

(defn a-throw [body-gen]
  (gen/fmap (fn [[msg body]] `(throw (ex-info ~msg {:value ~body})))
            (gen/tuple (gen/elements ["ex-a" "ex-b"]) body-gen)))
