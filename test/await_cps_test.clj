(ns await-cps-test
  (:refer-clojure :exclude [await])
  (:require [clojure.test :refer :all]
            [clojure.test.check :refer [quick-check]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]
            [await-cps :refer [async await]]
            [await-cps.impl :refer [has-async?]]
            [generators :as g]))

(def ^:dynamic side-effects nil)
(def timeout 200)

(defn ->async [form]
 `(async ~'respond ~'raise ~form))

(defn run-async* [timout async-form]
  (let [form `(let [result# (promise)
                    ~'respond #(deliver result# {:value %})
                    ~'raise #(deliver result# {:exception %})
                    ~@(mapcat #(vector % (str "undef-" %)) g/symbols)]
                ~async-form
                result#)
        result (binding [side-effects (atom nil)]
                  (merge (deref (eval form) timeout {:timeout timeout})
                         {:side-effects @side-effects}))]
  (if (:exception result)
    (try (throw (:exception result))
      (catch clojure.lang.ExceptionInfo t (assoc result :exception (.getMessage t))))
    result)))

(defn run-async [timeout form]
  (run-async* timeout (->async form)))

(defn run-sync [form]
  (with-redefs [await #(apply % %&)]
    (binding [side-effects (atom nil)]
      (merge
        (try {:value (eval `(let [~@(mapcat #(vector % (str "undef-" %)) g/symbols)] ~form))}
          (catch clojure.lang.ExceptionInfo t {:exception (.getMessage t)}))
        {:side-effects @side-effects}))))

(defn ->results [form]
  (let [[sync-res sync-error] (try [(run-sync form)] (catch Throwable t [nil t]))]
    (if sync-error
      {:code form
       :sync {:error sync-error}}
      (let [[compiled compilation-error] (try [(macroexpand (->async form))] (catch Throwable t [nil t]))]
        (if compiled
          {:code form
           :compiled compiled
           :sync sync-res
           :async (try (run-async* timeout compiled) (catch Throwable t {:error t}))}
          {:code form
           :sync sync-res
           :async {:error compilation-error}})))))

(defn verify-error? [results] ; For some reason try and loops exporessions mixed together throw these a lot
  (or (instance? VerifyError (:error (:sync results)))
      (instance? VerifyError (:error (:async results)))))

(defn code-prop [n code-gen predicate]
  (let [prop (for-all [{:keys [sync async]} 
                       (->> code-gen
                            (gen/such-that has-async?)
                            (gen/fmap ->results)
                            (gen/such-that #(not (verify-error? %))))]
               (predicate sync async))
        result (quick-check n prop)]
    (when (:fail result)
      (clojure.pprint/pprint (get-in result [:shrunk :smallest])))
    (not (:fail result))))

(defn effect [s] (swap! side-effects concat [s]))

(defn return
  ([s v] (effect s) v)
  ([s v r e] (effect s) (future (r v))))
(defn fail
  ([s v] (effect s) (throw (ex-info (str v) {})))
  ([s v r e] (effect s) (future (e (ex-info (str v) {})))))

(defn successful-async-call [body-gen]
  (gen/fmap (fn [value] `(await return ~(keyword (gensym)) ~value)) body-gen))

(defn successful-sync-call [body-gen]
  (gen/fmap (fn [value] `(return ~(keyword (gensym)) ~value)) body-gen))

(defn failing-async-call [body-gen]
  (gen/fmap (fn [value] `(await fail ~(keyword (gensym)) ~value)) body-gen))

(defn failing-sync-call [body-gen]
  (gen/fmap (fn [value] `(fail ~(keyword (gensym)) ~value)) body-gen))

(defn unique [body-gen]
  (gen/fmap (fn [b] `(do ~b ~(keyword (gensym)))) body-gen))

(def code-of-reliable-execution-order
  (let [wrapper
        #(gen/frequency [[10 (successful-async-call %)]
                         [10 (successful-sync-call %)]
                         [10 (failing-async-call %)]
                         [10 (failing-sync-call %)]
                         [10 (g/an-if %)]
                         [10 (g/a-do %)]
                         [10 (g/a-let %)]
                         [3 (g/a-loop %)]
                         [3 (g/a-case %)]
                         [3 (g/a-throw %)]
                         [5 (g/a-try %)]])]
    (->> (gen/one-of [g/a-symbol (gen/elements [:a :b :c])])
         (gen/recursive-gen wrapper))))

(defn same-side-effects-order-and-equal-return-value [sync-res async-res]
  (= sync-res async-res))

(deftest code-of-reliable-order
  (is (code-prop 200
                 code-of-reliable-execution-order
                 same-side-effects-order-and-equal-return-value)))

(def successful-code-of-unreliable-execution-order
  (let [wrapper
        #(gen/frequency [[10 (successful-async-call %)]
                         [10 (successful-sync-call %)]
                         [10 (gen/vector % 0 3)]
                         [10 (g/an-if %)]
                         [10 (g/a-do %)]
                         [10 (g/a-let %)]
                         [3 (g/a-loop %)]
                         [3 (g/a-case %)]
                         [5 (g/a-try %)]])]
    (->> (gen/one-of [g/a-symbol (gen/elements [:a :b :c])])
         (gen/recursive-gen wrapper))))

(defn out-of-order-side-effects-are-quivalent-and-return-value-equal [sync-res async-res]
  (= (update sync-res :side-effects sort)
     (update async-res :side-effects sort)))

(deftest successful-code-but-unreliable-order
  (is (code-prop 200
                 successful-code-of-unreliable-execution-order
                 out-of-order-side-effects-are-quivalent-and-return-value-equal)))

(def successful-code-of-unreliable-execution-order-including-maps-and-set
  (let [wrapper
        #(gen/frequency [[10 (successful-async-call %)]
                         [10 (successful-sync-call %)]
                         [10 (gen/vector % 0 3)]
                         [10 (gen/set (unique %) {:min-elements 0 :max-elements 3})]
                         [10 (gen/map (unique %) % {:min-elements 0 :max-elements 3})]
                         [10 (g/an-if %)]
                         [10 (g/a-do %)]
                         [10 (g/a-let %)]
                         [3 (g/a-loop %)]
                         [3 (g/a-case %)]
                         [5 (g/a-try %)]])]
     (->> (gen/one-of [g/a-symbol (gen/elements [:a :b :c :d :e])])
          (gen/recursive-gen wrapper))))

(deftest successful-code-but-unreliable-order-including-maps-and-sets
  (is (code-prop 200
                 successful-code-of-unreliable-execution-order-including-maps-and-set
                 out-of-order-side-effects-are-quivalent-and-return-value-equal)))

(def possibly-usuccessful-code
  (let [wrapper
        #(gen/frequency [[10 (successful-async-call %)]
                         [10 (successful-sync-call %)]
                         [10 (failing-async-call %)]
                         [10 (failing-sync-call %)]
                         [10 (gen/vector % 1 3)]
                         [10 (gen/set (unique %) {:min-elements 1 :max-elements 3})]
                         [10 (gen/map (unique %) % {:min-elements 1 :max-elements 3})]
                         [10 (g/an-if %)]
                         [10 (g/a-do %)]
                         [10 (g/a-let %)]
                         [3 (g/a-loop %)]
                         [3 (g/a-case %)]
                         [3 (g/a-throw %)]
                         [5 (g/a-try %)]])]
    (->> (gen/one-of [g/a-symbol (gen/elements [:a :b :c])])
         (gen/recursive-gen wrapper))))

(defn either-failure-or-out-of-order-side-effects-are-quivalent-and-return-value-equal [sync-res async-res]
  (or (and (:exception sync-res) (:exception async-res))
      (out-of-order-side-effects-are-quivalent-and-return-value-equal sync-res async-res)))

(deftest successful-and-unsuccessful-code
  (is (code-prop 200
                 possibly-usuccessful-code
                 either-failure-or-out-of-order-side-effects-are-quivalent-and-return-value-equal)))

(deftest no-awaits
  (is (= 1 (:value (run-async timeout `(let [a# 1] a#))))))

(deftest letfn-expression
  (is (= 2 (:value (run-async timeout
                              `(letfn [(a# [x# r# e#] (future (r# (inc x#))))
                                       (b# [y# r# e#] (async r# e# (await a# y#)))]
                                 (await b# 1)))))))

(def ^:dynamic dynamic-var :globally-bound)

(defn some-async [v r e]
  (.start (new Thread (reify Runnable (run [_] (r v))))))

(deftest local-bindings
  (is (= :locally-bound
         (binding [dynamic-var :locally-bound]
           (:value (run-async timeout `(do (await some-async :a) dynamic-var)))))))

(defn value ([v] v) ([v r e] (future (r v))))

(deftest java-interop
  (is (= 1 (:value (run-async timeout `(Integer. (await value "1"))))))
  (is (= 5 (:value (run-async timeout `(. (await value "works") length)))))
  (is (= 1 (:value (run-async timeout `(Integer/parseInt "1")))))

  (is (= 8 (:value (run-async timeout `(set! (. (await value (java.awt.Point. 1 2)) -y) 8)))))
  (is (= 8 (:value (run-async timeout `(set! (. (java.awt.Point. 1 2) -y) (await value 8)))))))

(deftest passthrough-symbols
  (is (= `value (:value (run-async timeout `(await value (quote value))))))
  (is (= #'value (:value (run-async timeout `(await value (var value)))))))
