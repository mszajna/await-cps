(ns await-cps
  "async/await for continuation-passing style (CPS) functions.

   This library delivers async/await expressions for use with asynchronous
   functions that take resolve and raise callbacks as the last parameters
   (e.g. ring, clj-http...)."
  (:refer-clojure :exclude [await])
  (:require [await-cps.ioc :refer [invert]]))

(defn run-once
  "Wraps the functions provided so that only one will ever be invoked and
   at most once. Guarantees the release of closed-over references after
   the first call of either."
  [resolve raise]
  (let [ctx (atom [resolve raise])
        call (fn [sel]
               (fn [v] (let [c @ctx]
                         (when (zero? (swap! ctx #(if (vector? %) 0 (inc %))))
                           ((sel c) v)))))]
    [(call first) (call second)]))

(defmacro ^:no-doc with-binding-frame [frame & body]
 `(let [original-frame# (clojure.lang.Var/getThreadBindingFrame)]
    (clojure.lang.Var/resetThreadBindingFrame ~frame)
    (try
     ~@body
      (finally
        (clojure.lang.Var/resetThreadBindingFrame original-frame#)))))

(defn ^:no-doc do-await
  [r e f & args]
  (let [call-site-frame (clojure.lang.Var/getThreadBindingFrame)
        [resolve raise] (run-once
                          (fn [v] (with-binding-frame call-site-frame
                                    (try (r v)
                                      (catch Throwable t (e t)))))
                          (fn [t] (with-binding-frame call-site-frame
                                    (e t))))]
    (apply f (concat args [resolve raise]))))

(defn await
  "Awaits asynchronous execution of continuation-passing style function f,
   applying it to args provided plus two extra callback functions: resolve and
   raise. Returns the value passed to resolve or throws the exception passed
   to raise. Must execute in an async block."
  [f & args]
  (throw (new IllegalStateException "await called outside async block")))

(def ^:no-doc terminal-symbols
  {`await `do-await})

(defn default-log-raise-exception
  [^Throwable t]
  (println "Uncaught exception executing raise callback:" (.getMessage t)))

(def ^:dynamic *log-raise-exception*
  "If the raise callback in an async block throws, the exception will be logged
   with this function.

   WARNING: If this function throws too the exception will be silently swallowed."
  default-log-raise-exception)

(defmacro async
  "Executes the body calling resolve with the result. Exceptions thrown by
   either the body or the resolve function will be raised calling raise.

   Executes in the calling thread up to the first await clause. Execution is
   then resumed in the thread awaited function run its resolve callback in.
   This may be the calling thread."
  [resolve raise & body]
  (let [r (gensym)
        e (gensym)]
   `(let [~e #(try (~raise %)
                (catch Throwable t# (try (*log-raise-exception* t#)
                                      (catch Throwable _#))))
          ~r #(try (~resolve %) (catch Throwable t# (~e t#)))]
      (with-binding-frame (clojure.lang.Var/getThreadBindingFrame)
        (try
         ~(invert {:r r :e e :terminal-symbols terminal-symbols} `(do ~@body))
          (catch Throwable t# (~e t#))))
      nil)))

(defmacro fn-async
  "Same as fn but adds &resolve and &raise params and executes the body
   in an async block using those as callbacks. Only one arity is allowed."
  {:arglists '([name? [params*] body])}
  [& args]
  (let [[a & [b & cs :as bs]] args
        [extras params body]
        (if (symbol? a)
          [[a] b cs] [nil a bs])]
   `(fn ~name [~@params ~'&resolve ~'&raise]
      (async ~'&resolve ~'&raise ~@body))))

(defmacro defn-async
  "Same as defn but adds &resolve and &raise params and executes the body
   in an async block using those as callbacks. Only one arity is allowed."
  {:arglists '([name doc-string? attr-map? [params*] body])}
  [name & args]
  (let [[a & [b & [c & ds :as cs] :as bs]] args
        [extras params body]
        (if (string? a)
          (if (map? b) [[a b] c ds] [[a] b cs])
          (if (map? a) [[a] b cs] [nil a bs]))]
  `(defn ~name ~@extras [~@params ~'&resolve ~'&raise]
      (async ~'&resolve ~'&raise ~@body))))
