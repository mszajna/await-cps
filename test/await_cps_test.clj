(ns await-cps-test
  (:refer-clojure :exclude [await])
  (:require [clojure.test :refer :all]
            [clojure.test.check :refer [quick-check]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]
            [await-cps :refer [async await]]
            [await-cps.impl :refer [has-async?]]
            [generators :as g]))

(def side-effects (atom nil))
(def timeout 100)

(defn run-async [form timout]
  (reset! side-effects nil)
  (let [form `(let [result# (promise)
                    ~@(interpose "undef" g/symbols) "undef"]
                (async #(deliver result# %) #(deliver result# [:ex (.getMessage ^Exception %)]) ~form)
                result#)
        result (deref (eval form) timeout :timeout)]
    [@side-effects result]))

(defn run-sync [form]
  (reset! side-effects nil)
  (with-redefs [await #(apply % %&)]
    (let [result (try (eval `(let [~@(interpose "undef" g/symbols) "undef"] ~form))
                      (catch clojure.lang.ExceptionInfo t [:ex (.getMessage ^Exception t)]))]
      [@side-effects result])))

(defn explain [form]
  (clojure.pprint/pprint
    {:code form
     :compiled (clojure.walk/macroexpand-all `(async r e ~form))
     :sync (try (run-sync form) (catch Throwable t [:exception (.getMessage ^Exception t)]))
     :async (try (run-async form timeout) (catch Throwable t [:exception (.getMessage ^Exception t)]))}))

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

(defn side-effectful-call [body-gen]
  (gen/fmap (fn [[async? return? value]]
              `(~@(when async? `[await])
                ~(if return? `return `fail)
                ~(keyword (str "side-effect-" (gensym))) ~value))
            (gen/tuple gen/boolean gen/boolean body-gen)))

(defn arbitrary-code [val-gen]
  (gen/recursive-gen #(gen/one-of [(side-effectful-call %) (g/clojure-code %)]) val-gen))

(def side-effects-and-return-value-are-equivalent
  (for-all [[code sync-result]
            (->> (arbitrary-code (gen/one-of [g/a-symbol (gen/elements [:a :b :c])]))
                 (gen/fmap (fn [code] [code (try (run-sync code) (catch Throwable t :throws))]))
                 (gen/such-that (fn [[_ result]] (not= result :throws))))]
    (let [async-result (run-async code timeout)]
      (= sync-result async-result))))

(deftest side-effects-and-return-value
  (let [{:keys [result shrunk]}
        (quick-check 20 side-effects-and-return-value-are-equivalent)]
    (when-not (is result "Expect sync and async code to invoke the same side effects and return same value")
      (explain shrunk))))

(def ^:dynamic dynamic-var :globally-bound)

(defn some-async [v r e]
  (.start (new Thread (reify Runnable (run [_] (r v))))))

(deftest local-bindings
  (let [form `(do (await some-async :a) dynamic-var)]
    (is (= [nil :locally-bound]
           (binding [dynamic-var :locally-bound]
             (run-async form timeout))))))
