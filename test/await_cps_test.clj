(ns await-cps-test
  (:refer-clojure :exclude [await])
  (:require [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]
            [await-cps :refer [async await has-async?]]
            [generators :as g]))

; The goal of this library is to enable writing CPS (continuation-passing style)
; code preserving the flow of synchronous code. It achieves that elevating
; the CPS calls deep in a form all the way up

(def side-effects (atom nil))
(def timeout 50)

(defn run-async [form]
  (reset! side-effects nil)
  (let [form `(let [result# (promise)]
                (async #(deliver result# %) #(deliver result# [:ex (.getMessage %)]) ~form)
                result#)
        result (try (deref (eval form) timeout :timeout)
                    (catch clojure.lang.ExceptionInfo t [:ex (.getMessage t)])
                    (catch IllegalArgumentException t [:ex (.getMessage t)]))]
    [@side-effects result]))

(defn run-sync [form]
  (reset! side-effects nil)
  (with-redefs [await #(apply % %&)]
    (let [result (try (eval form)
                      (catch clojure.lang.ExceptionInfo t [:ex (.getMessage t)])
                      (catch IllegalArgumentException t [:ex (.getMessage t)]))]
      [@side-effects result])))

(defn sync-equiv [gen-form]
  (for-all [form (gen/such-that has-async? gen-form)]
    (= (run-sync form) (run-async form))))

(defn explain [form]
  (clojure.pprint/pprint
    {:compiled (await-cps/async* 'r 'e form)
     :sync (run-sync form)
     :async (run-async form)}))

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
  (sync-equiv (arbitrary-code (gen/elements [:a :b :c :d]))))

(def bindable (gen/fmap symbol gen/keyword))

(defspec let-bindings-are-available-to-async-code-downstream 20
  (sync-equiv (gen/let [a-sym bindable
                        a-val (arbitrary-code (gen/elements [:a :b :c :d]))
                        b-sym bindable
                        b-val (arbitrary-code (gen/return a-sym))
                        calls (gen/vector (arbitrary-code (gen/elements [a-sym b-sym])) 0 3)]
                `(let [~a-sym ~a-val
                      ~b-sym ~b-val] ~@calls))))
