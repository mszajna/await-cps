(ns await-cps-test
  (:refer-clojure :exclude [await])
  (:require [clojure.test :refer :all]
            [clojure.test.check :refer [quick-check]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]
            [await-cps :refer [async await afn defn-async]]
            [await-cps.ioc :refer [has-terminators?]]
            [await-cps.generators :as g]))

(def ^:dynamic side-effects nil)
(def timeout 200)

(defn ->async [form]
 `(async ~'respond ~'raise ~form))

(defn mock-await [async-fn & args]
  (let [result (promise)
        respond #(deliver result {:value %})
        raise #(deliver result {:exception %})]
    (apply async-fn (concat args [respond raise]))
    (let [{:keys [value exception timeout]}
          (deref result timeout {:timeout timeout})]
      (cond value value
            exception (throw exception)
            timeout (throw (new java.util.concurrent.TimeoutException (str "Timed out after " timeout)))))))

(defn ->sync-fn [form]
  (eval
   `(fn []
      (try {:value (let [~@(mapcat #(vector % (str "undef-" %)) g/symbols)] ~form)}
        (catch clojure.lang.ExceptionInfo t# {:exception (.getMessage t#)})))))

(defn ->async-fn [timeout async-form]
  (eval
   `(fn []
      (let [result# (promise)
            ~'respond #(deliver result# {:value %})
            ~'raise #(deliver result# {:exception %})
            ~@(mapcat #(vector % (str "undef-" %)) g/symbols)]
        ~async-form
        (deref result# ~timeout {:timeout timeout})))))

(defn run-async* [timeout async-form]
  (let [form `(let [result# (promise)
                    ~'respond #(deliver result# {:value %})
                    ~'raise #(deliver result# {:exception %})
                    ~@(mapcat #(vector % (str "undef-" %)) g/symbols)]
                ~async-form
                result#)
        result (binding [side-effects (atom nil)]
                  (merge ((->async-fn timeout async-form))
                         {:side-effects @side-effects}))]
  (if (:exception result)
    (try (throw (:exception result))
      (catch clojure.lang.ExceptionInfo t (assoc result :exception (.getMessage t))))
    result)))

(defn run-async [timeout form]
  (run-async* timeout (->async form)))

(defn run-sync [form]
  (with-redefs [await ;#(apply % %&)]
                      mock-await]
    (binding [side-effects (atom nil)]
      (merge
        ((->sync-fn form))
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

(defn ->results-gen [code-gen]
  (->> code-gen
       (gen/such-that #(has-terminators? % {:terminators {`await nil}}))
       (gen/fmap ->results)
       (gen/such-that #(not (or (instance? VerifyError (:error (:sync %)))
                                (instance? VerifyError (:error (:async %))))))))

(defn code-prop [n code-gen predicate]
  (let [prop (for-all [{:keys [sync async]} (->results-gen code-gen)]
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

(defn unique [body-gen]
  (gen/fmap (fn [b] `(do ~b ~(keyword (gensym)))) body-gen))

(defn sync-return [s v]
  (effect s)
  v)

(defn async-return-future [s v r e]
  (effect s)
  (future (r v)))

(defn async-return-immediate [s v r e]
  (effect s)
  (r v))

(defn ->throwable [v] (ex-info (str v) {}))

(defn sync-throw [s v]
  (effect s)
  (throw (->throwable v)))

(defn async-throw-future [s v r e]
  (effect s)
  (future (e (->throwable v))))

(defn async-throw-immediate [s v r e]
  (effect s)
  (throw (->throwable v)))

(defn async-singal-immediate [s v r e]
  (effect s)
  (e (->throwable v)))

(defn successful-async-call [body-gen]
  (gen/fmap (fn [[immediate? value]]
             `(await ~(if immediate?
                        `async-return-immediate
                        `async-return-future)
                     ~(keyword (gensym))
                     ~value))
            (gen/tuple gen/boolean body-gen)))

(defn failing-async-call [body-gen]
  (gen/fmap (fn [[mode value]]
             `(await ~(case mode
                        0 `async-throw-future
                        1 `async-throw-immediate
                        2 `async-singal-immediate)
                     ~(keyword (gensym))
                     ~value))
            (gen/tuple (gen/choose 0 2) body-gen)))

(defn successful-sync-call [body-gen]
  (gen/fmap (fn [value] `(sync-return ~(keyword (gensym)) ~value)) body-gen))

(defn failing-sync-call [body-gen]
  (gen/fmap (fn [value] `(sync-throw ~(keyword (gensym)) ~value)) body-gen))

; (defn doesnt-throw-verify-errors? [code]
;   (try (run-sync code) true (catch VerifyError t false)))

(def code-of-reliable-execution-order
  (let [wrapper
        #(gen/frequency [[10 (successful-async-call %)]
                         [10 (successful-sync-call %)]
                         [10 (failing-async-call %)]
                         [10 (failing-sync-call %)]

                         [10 (gen/vector % 0 3)]
                         [10 (gen/set (unique %) {:min-elements 0 :max-elements 3})]
                         [10 (gen/map (unique %) % {:min-elements 0 :max-elements 3})]

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

(deftest test-code-of-reliable-execution-order
  (is (code-prop 2000
                 code-of-reliable-execution-order
                 same-side-effects-order-and-equal-return-value)))

(deftest no-awaits
  (is (= 1 (:value (run-async timeout `(let [a# 1] a#))))))

(deftest letfn-expression
  (is (= 2 (:value (run-async timeout
                              `(letfn [(a# [x# r# e#] (future (r# (inc x#))))
                                       (b# [y# r# e#] (async r# e# (await a# y#)))]
                                 (await b# 1)))))))

(deftest shadowing-symbols
  (binding [*ns* (find-ns 'await-cps-test)]
    ;; Here we shadow the `await` macro with a local function
    (is (= 1 (:value (run-async timeout `(let [~'await (constantly 1)]
                                           (~'await (fn [r# e#] (r# 2))))))))
    ;; Here we shadow the `and` macro with a local function
    (is (= 1 (:value (run-async timeout `(let [~'and (constantly 1)]
                                           (~'and (await (fn [r# e#] (r# 2)))))))))
    ;; Here we use `letfn` to introduce the binding
    (is (= 1 (:value (run-async timeout `(letfn [(~'await [x#] 1)]
                                           (~'await (fn [r# e#] (r# 2))))))))
    ;; Here we use `loop` to introduce the binding
    (is (= 1 (:value (run-async timeout `(loop [~'await (constantly 1)]
                                           (~'await (fn [r# e#] (r# 2))))))))
    ;; Here we use `catch` to introduce the binding
    (is (thrown? ClassCastException ;; can't call Exception as function
                 (:exception (run-async timeout `(try
                                                   (throw (Exception.))
                                                   (catch Exception ~'await
                                                     (~'await (fn [r# e#] (r# 1)))))))))))

(def ^:dynamic dynamic-var :globally-bound)

(defn some-async [v r e]
  (.start (new Thread (reify Runnable (run [_] (r v))))))

(deftest local-bindings
  (is (= :locally-bound
         (binding [dynamic-var :locally-bound]
           (:value (run-async timeout `(do (await some-async :a) dynamic-var))))))
  (is (= :locally-bound
         (:value (run-async timeout `(binding [dynamic-var :locally-bound] (await some-async dynamic-var)))))))

(defn value ([v] v) ([v r e] (future (r v))))

(deftest java-interop
  (is (= 1 (:value (run-async timeout `(Integer. ^String (await value "1"))))))
  (is (= 5 (:value (run-async timeout `(. ^String (await value "works") length)))))
  (is (= 1 (:value (run-async timeout `(Integer/parseInt "1")))))

  (is (= 8 (:value (run-async timeout `(set! (. (await value (java.awt.Point. 1 2)) -y) 8)))))
  (is (= 8 (:value (run-async timeout `(set! (. (java.awt.Point. 1 2) -y) (await value 8)))))))

(deftest passthrough-symbols
  (is (= `value (:value (run-async timeout `(await value (quote value))))))
  (is (= #'value (:value (run-async timeout `(await value (var value)))))))

(deftest test-fn-async
  (is (= 1 (:value (run-async timeout `(await (afn [] (await value 1)))))))
  (is (= 0 (:value (run-async timeout `(await (afn [a#]
                                                (if (> a# 0)
                                                    (recur (await async-return-immediate nil (dec a#))) a#))
                                              10000))))))

(deftest test-loop
  (is (= 0 (:value (run-async timeout `(loop [a# 10000] (if (> a# 0) (recur (dec a#)) a#))))))
  (is (= 0 (:value (run-async (* 10 timeout) `(loop [a# 10000] (if (> a# 0) (recur (await value (dec a#))) a#))))))
  (is (= 0 (:value (run-async timeout `(loop [a# 100000] (if (> a# 0) (recur (await async-return-immediate nil (dec a#))) a#))))))
  (is (= 0 (:value (run-async (* 10 timeout)
                              `(loop [a# 10000]
                                (await async-return-future nil nil)
                                (if (> a# 0) (recur (await async-return-immediate nil (dec a#))) a#)))))))

(defn benchmark [n]
  (let [{:keys [code compiled]} (gen/generate (->results-gen code-of-reliable-execution-order) 200)
        sync-fn (->sync-fn code)
        async-fn (->async-fn timeout compiled)
        sync-res (do (println "running sync")
                     (with-redefs [await mock-await]
                       (time (dotimes [i n]
                               (binding [side-effects (atom nil)] (sync-fn))))))
        async-res (do (println "running async")
                      (time (dotimes [i n]
                              (binding [side-effects (atom nil)] (async-fn)))))]
    (= sync-res async-res)))
