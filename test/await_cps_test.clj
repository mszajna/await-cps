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

(defn run-async [timout form]
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
     :async (try (run-async timeout form) (catch Throwable t [:exception (.getMessage ^Exception t)]))}))

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

(defn code-of-reliable-execution-order [body-gen]
  (gen/frequency [[20 (side-effectful-call body-gen)]
                  [10 (gen/vector body-gen 0 3)]
                  [10 (g/an-if body-gen)]
                  [10 (g/a-do body-gen)]
                  [10 (g/a-let body-gen)]
                  [3 (g/a-loop body-gen)]
                  [3 (g/a-case body-gen)]
                  [3 (g/a-throw body-gen)]
                  [5 (g/a-try body-gen)]]))

(def side-effects-and-return-value-are-equivalent
  (for-all [[code sync-result]
            (->> (gen/recursive-gen code-of-reliable-execution-order
                                    (gen/one-of [g/a-symbol (gen/elements [:a :b :c])]))
                 (gen/fmap (fn [code] [code (try (run-sync code) (catch Throwable t :throws))]))
                 (gen/such-that (fn [[_ result]] (not= result :throws))))]
    (let [async-result (run-async timeout code)]
      (= sync-result async-result))))

(deftest side-effects-and-return-value
  (let [{:keys [result shrunk]}
        (quick-check 20 side-effects-and-return-value-are-equivalent)]
    (when-not (is result "Expect sync and async code to invoke the same side effects and return same value")
      (explain (:smallest shrunk)))))

(deftest maps-and-sets
  (is (= [#{:a :b} {1 2 3 4}]
         (update (run-async timeout `{(await return :a 1) 2 (await return :b 3) 4})
                 0 set)))
  (is (= [#{:a :b} #{1 2}]
         (update (run-async timeout `#{(await return :a 1) (await return :b 2)})
                 0 set))))

(deftest letfn-expression
  (is (= [nil 2] (run-async timeout `(letfn [(a# [x# r# e#] (future (r# (inc x#))))
                                             (b# [y# r# e#] (async r# e# (await a# y#)))]
                                       (await b# 1))))))

(def ^:dynamic dynamic-var :globally-bound)

(defn some-async [v r e]
  (.start (new Thread (reify Runnable (run [_] (r v))))))

(deftest local-bindings
  (is (= [nil :locally-bound]
         (binding [dynamic-var :locally-bound]
           (run-async timeout `(do (await some-async :a) dynamic-var))))))

(defn value [v r e] (future (r v)))

(deftest java-interop
  (is (= [nil 1] (run-async timeout `(Integer. (await value "1")))))
  (is (= [nil 5] (run-async timeout `(. (await value "works") length))))
  (is (= [nil 1] (run-async timeout `(Integer/parseInt "1"))))

  (is (= [nil 8] (run-async timeout `(set! (. (await value (java.awt.Point. 1 2)) -y) 8))))
  (is (= [nil 8] (run-async timeout `(set! (. (java.awt.Point. 1 2) -y) (await value 8))))))

(deftest passthrough-symbols
  (is (= [nil `value] (run-async timeout `(await value (quote value)))))
  (is (= [nil #'value] (run-async timeout `(await value (var value))))))
