(ns gen.yield
  (:require [await-cps.ioc :refer [coroutine]]))

(defn ^:no-doc do-yield [r e v]
  (let [br (bound-fn* r)] ; TODO: use fast binding
    (cons v (lazy-seq (br nil)))))

(defn yield
  [value]
  (throw (new IllegalStateException
              "yield called outside gen")))

(def ^:no-doc ignore-return (constantly nil))
(defn ^:no-doc rethrow [t] (throw t))

(defmacro gen
  [& body]
 `((coroutine {yield do-yield} ~@body) ignore-return rethrow))    
