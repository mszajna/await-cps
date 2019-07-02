(ns await-cps
  "async/await for continuation-passing style (CPS) functions.

   This library delivers async/await expressions for use with asynchronous
   functions that take resolve and raise callbacks as the last parameters
   (e.g. ring, clj-http...)."
  (:refer-clojure :exclude [await])
  (:require [await-cps.ioc :refer [async*]]))

(def ^:dynamic ^java.util.concurrent.ScheduledExecutorService
     *scheduled-executor-service*
  (java.util.concurrent.Executors/newSingleThreadScheduledExecutor))

(defn timeout
  "After timeout milliseconds elapsed will call resolve with timeout-val
   if provided, otherwise will call raise with a TimeoutException."
 ([timeout-ms resolve raise]
  (.schedule *scheduled-executor-service*
             ^Runnable
             (fn []
               (future
                 (raise (new java.util.concurrent.TimeoutException
                             (str "Done waiting after " timeout-ms " ms")))))
             ^Long
             timeout-ms
             java.util.concurrent.TimeUnit/MILLISECONDS))
 ([timeout-ms timeout-val resolve raise]
  (.schedule *scheduled-executor-service*
             ^Runnable
             (fn [] (future (resolve timeout-val)))
             ^Long
             timeout-ms
             java.util.concurrent.TimeUnit/MILLISECONDS)))

(defn run-once
  "Wraps the functions provided so that only one will ever be invoked and
   at most once. Guarantees the release of closed-over references after
   the first call of either."
  [resolve raise]
  (let [ctx (atom [0 resolve raise])
        upd (fn [[i]] [(inc i)])]
    [(fn [v] (let [[_ r] @ctx]
               (when (= 1 (first (swap! ctx upd))) (r v))))
     (fn [v] (let [[_ _ e] @ctx]
               (when (= 1 (first (swap! ctx upd))) (e v))))]))

(defmacro ^:no-doc with-binding-frame [frame & body]
 `(let [original-frame# (clojure.lang.Var/getThreadBindingFrame)]
    (clojure.lang.Var/resetThreadBindingFrame ~frame)
    (try
     ~@body
      (finally
        (clojure.lang.Var/resetThreadBindingFrame original-frame#)))))

(defn ^:no-doc do-await
  [r e & args]
  (let [[timeout-ms f & args] (if (int? (first args)) args (concat [nil] args))
        call-site-frame (clojure.lang.Var/getThreadBindingFrame)
        cont (fn [v] (with-binding-frame call-site-frame
                       (try (r v)
                         (catch Throwable t (e t)))))
        [resolve raise] (run-once cont e)]
    (apply f (concat args [resolve raise]))
    (when timeout-ms (timeout timeout-ms resolve raise))))

(defn await
  "Awaits asynchronous execution of continuation-passing style function f,
   applying it to args provided plus two extra callback functions: resolve and
   raise. Returns the value passed to resolve or throws the exception passed
   to raise. Must execute in an async block."
  {:arglists '([timeout? f & args])}
  [& args]
  (throw (new IllegalStateException "await called outside async block")))

(def terminal-symbols
  {`await `do-await})

(defmacro async
  "Executes the body calling resolve with the result if successful or
   calling raise with the exception. If the body contains await clauses
   the execution will not block the calling thread."
  [resolve raise & body]
  (let [r (gensym)
        e (gensym)]
    ; TODO: Sanitize for monitor-* expressions.
    ;       No point sanitizing unexpanded but expanding
    ;       yields undeterministic evalutation for maps and sets
    ; (sanitize body)
    ; TODO: What should happen when resolve or raise throw?
    ;       It feels that doesn't fit CPS model (kind of the whole point of this lib).
    ;       Perhaps enforce they don't wrapping in a try catch + warn?
   `(let [~r ~resolve
          ~e ~raise]
      (with-binding-frame (clojure.lang.Var/getThreadBindingFrame)
        (try
         ~(async* {:r r :e e :terminal-symbols terminal-symbols} `(do ~@body))
          (catch Throwable t# (~e t#))))
      nil)))

(defmacro fn-async
  "Same as fn but adds &resolve and &raise params and executes the body
  in an async block. Only one arity is allowed."
  {:arglists '([name? [params*] body])}
  [name & args]
  (let [[a & [b & [c & ds :as cs] :as bs]] args
        [extras params body]
        (if (string? a)
          [[a] b cs] [nil a bs])]
   `(fn ~@extras [~@params ~'&resolve ~'&raise]
      (async ~'&resolve ~'&raise ~@body))))

(defmacro defn-async
  "Same as defn but adds &resolve and &raise params and executes the body
  in an async block. Only one arity is allowed."
  {:arglists '([name doc-string? attr-map? [params*] body])}
  [name & args]
  (let [[a & [b & [c & ds :as cs] :as bs]] args
        [extras params body]
        (if (string? a)
          (if (map? b) [[a b] c ds] [[a] b cs])
          (if (map? a) [[a] b cs] [nil a bs]))]
  `(defn ~name ~@extras [~@params ~'&resolve ~'&raise]
      (async ~'&resolve ~'&raise ~@body))))
