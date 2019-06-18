(ns await-cps-test
  (:refer-clojure :exclude [await])
  (:require [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]
            [await-cps :refer [async await]]
            [await-cps.impl :refer [has-async?]]
            [generators :as g]))

(def side-effects (atom nil))
(def timeout 50)

(defn run-async [form]
  (reset! side-effects nil)
  (let [form `(let [result# (promise)
                    ~@(interpose "undef" g/symbols) "undef"]
                (async #(deliver result# %) #(deliver result# [:ex (.getMessage ^Exception %)]) ~form)
                result#)
        result (try (deref (eval form) timeout :timeout)
                    (catch clojure.lang.ExceptionInfo t [:ex (.getMessage ^Exception t)])
                    (catch IllegalArgumentException t [:ex (.getMessage ^Exception t)]))]
    [@side-effects result]))

(defn run-sync [form]
  (reset! side-effects nil)
  (with-redefs [await #(apply % %&)]
    (let [result (try (eval `(let [~@(interpose "undef" g/symbols) "undef"] ~form))
                      (catch clojure.lang.ExceptionInfo t [:ex (.getMessage ^Exception t)])
                      (catch IllegalArgumentException t [:ex (.getMessage ^Exception t)]))]
      [@side-effects result])))

(defn sync-equiv [gen-form]
  (for-all [form (gen/such-that has-async? gen-form)]
    (= (run-sync form) (run-async form))))

(defn explain [form]
  (clojure.pprint/pprint
    {:compiled (clojure.walk/macroexpand-all `(async r e ~form))
     :sync (try (run-sync form) (catch Throwable t [:exception (.getMessage ^Exception t)]))
     :async (try (run-async form) (catch Throwable t [:exception (.getMessage ^Exception t)]))}))

(defn effect [s] (swap! side-effects concat [s]))

(defn return
  ([s v] (effect s) v)
  ([s v r e] (effect s) (future (r v))))
(defn fail
  ([s v] (effect s) (throw (ex-info (str v) {})))
  ([s v r e] (effect s) (future (e (ex-info (str v) {})))))

(defn fn-arity-1 []
  (gen/fmap (fn [f] (f (keyword (str "side-effect-" (gensym)))))
    (gen/elements [(fn [s] `(return ~s)) (fn [s] `(await return ~s))
                   (fn [s] `(fail ~s)) (fn [s] `(await fail ~s))])))

(defn returning [v]
  (gen/fmap (fn [f] `(~@f ~v)) (fn-arity-1)))

; Test cases

(defn side-effectful-call [body-gen]
  (gen/fmap (fn [[async? return? value]]
              `(~@(when async? `[await])
                ~(if return? `return `fail)
                ~(keyword (str "side-effect-" (gensym))) ~value))
            (gen/tuple gen/boolean gen/boolean body-gen)))

(defn arbitrary-code [val-gen]
  (gen/recursive-gen #(gen/one-of [(side-effectful-call %) (g/clojure-code %)]) val-gen))

(defspec arbitrary-async-code-evaluation-equivalent-to-sync 200
  (sync-equiv (arbitrary-code (gen/one-of [g/a-symbol (gen/elements [:a :b :c])]))))
