(ns await-cps.interop-test
  (:refer-clojure :exclude [await])
  (:require [clojure.test :refer :all]
            [await-cps :refer [afn await either-promise]]
            [await-cps.java :as j]
            [manifold.deferred :as d]
            [clojure.core.async :as a])
  (:import java.util.concurrent.CompletableFuture
           java.util.concurrent.TimeUnit
           java.util.concurrent.TimeoutException
           java.util.concurrent.ExecutionException))

(defn await-at-most! [ms cps-fn & args]
  (let [p (apply either-promise cps-fn args)
        [v t] (deref p ms [nil (new TimeoutException)])]
    (if t (throw t) v)))

(deftest use-java-cf-in-async-function
  (let [cf (new CompletableFuture)]
    (.complete cf true)
    (is (await-at-most! 200 (afn [] (await j/complete cf)))))
  (let [cf (new CompletableFuture)]
    (.completeExceptionally cf (ex-info "Success" {}))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Success"
          (await-at-most! 200 (afn [] (await j/complete cf)))))))

(deftest use-cps-with-java-cf
  (let [cf (j/future-call (afn [] true))]
    (is (deref cf 200 :timeout)))
  (let [cf (j/future-call (afn [] (throw (ex-info "Success" {}))))]
    (is (thrown-with-msg? ExecutionException #"Success"
          (deref cf 200 :timeout)))))

(deftest use-manifold-deferred-in-async-function
  (let [d (d/success-deferred true)]
    (is (await-at-most! 200 (afn [] (await d/on-realized d)))))
  (let [d (d/error-deferred (ex-info "Success" {}))]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Success"
          (await-at-most! 200 (afn [] (await d/on-realized d)))))))

(deftest use-cps-with-manifold-deferred
  (let [d (d/chain (j/future-call (afn [] true)))]
    (is (deref d 200 :timeout)))
  (let [d (d/chain (j/future-call (afn [] (throw (ex-info "Success" {})))))]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Success"
          (deref d 200 :timeout)))))

(deftest use-core-async-channel-in-async-function
  (let [ch (a/chan 1)]
    (is (await-at-most! 200
          (afn [] (when (await a/put! ch true) (await a/take! ch))))))
  (let [ch (a/chan)]
    (is (nil? (await-at-most! 200
                (afn [] (a/close! ch) (await a/take! ch)))))))
