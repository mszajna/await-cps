(ns await-cps.java
  "Interoperability with Java 8 CompletableFuture."
  (:refer-clojure :exclude [future-call])
  (:import java.util.function.BiFunction
           java.util.concurrent.CompletableFuture))

(defn future-call
  "Applies cps-fn to args plus two continuations that complete the
   CompletableFuture returned successfully or exceptionally."
  [cps-fn & args]
  (let [cf (new CompletableFuture)
        f (apply partial cps-fn args)
        r #(.complete cf %)
        e #(.completeExceptionally cf %)]
    (try (f r e)
      (catch Throwable t (e t)))
    cf))

(defn complete
  "When the CompletableFuture completes successfully, calls resolve with the
   value. If it completes exceptionally, calls raise with the exception.
   Use with await: (await complete cf)"
  [^CompletableFuture cf resolve raise]
  (.handle
    cf
    (reify BiFunction
      (apply [_ v t]
        (if t
          (raise t)
          (resolve v))))))

(def ^{:no-doc true} completed complete)
