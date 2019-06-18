(ns await-cps
  "async/await for continuation-passing style (CPS) functions.

   This library delivers async/await expressions for use with asynchronous
   functions that take resolve and raise callbacks as the last parameters
   (e.g. ring, clj-http...)."
  (:refer-clojure :exclude [await defn])
  (:require [clojure.core :as clj]
            [await-cps.impl :refer [async*]]))

(clj/defn await
  "Awaits asynchronous execution of continuation-passing style function f,
   applying it to args provided plus two extra callback functions: resolve and
   raise. Returns the value passed to resolve or throws the exception passed
   to raise. Must execute in an async block."
  [f & args]
  (throw (new IllegalStateException "await called outside async block")))

(defmacro async
  "Executes the body calling resolve with the result if successful or
   calling raise with the exception. If the body contains await clauses
   the execution will not block the calling thread."
  [resolve raise & body]
  (let [r (gensym)
        e (gensym)]
   `(let [~r ~resolve
          ~e ~raise]
      (try
       ~(async* {:r r :e e} `(do ~@body))
        (catch Throwable t# (~e t#)))
      nil)))

(defmacro defn
  "Same as defn but adds &resolve and &raise params and executes the body
   in an async block. Only one arity is allowed."
  ^{:arglists '([name doc-string? attr-map? [params*] body])}
  [name & args]
  (let [[a & [b & [c & ds :as cs] :as bs]] args
        [extras params body]
        (if (string? a)
          (if (map? b) [[a b] c ds] [[a] b cs])
          (if (map? a) [[a] b cs] [nil a bs]))]
   `(clj/defn ~name ~@extras [~@params ~'&resolve ~'&raise]
      (async ~'&resolve ~'&raise ~@body))))
