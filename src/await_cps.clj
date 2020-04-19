(ns await-cps
  "async/await syntax for functions that take a successful- and an exceptional
   callback in the last two arguments, a pattern known as continuation-passing
   style (CPS) and popularised by Ring and clj-http."
  (:refer-clojure :exclude [await bound-fn])
  (:require [await-cps.ioc :refer [coroutine]]))

(defn ^:no-doc bound-fn
  [f]
  (let [bound-frame (clojure.lang.Var/getThreadBindingFrame)]
    (fn [& args]
      (let [call-site-frame (clojure.lang.Var/getThreadBindingFrame)]
        (clojure.lang.Var/resetThreadBindingFrame bound-frame)
        (try
          (apply f args)
          (finally
            (clojure.lang.Var/resetThreadBindingFrame call-site-frame)))))))

(defn ^:no-doc do-await
  [r e f & args]
  (let [state (atom [:start])
        resolve (fn [v] (let [[[before r']]
                              (swap-vals! state
                                          #(case (first %)
                                             :start [:resolved v]
                                             :async [:completed]
                                             %))]
                          (when (= before :async) (r' v))))
        raise (fn [t] (let [[[before _ e']]
                            (swap-vals! state
                                        #(case (first %)
                                           :start [:raised t]
                                           :async [:completed]
                                           %))]
                        (when (= before :async) (e' t))))]
    (apply f (concat args [resolve raise]))
    (let [run (bound-fn trampoline)
          [[before x]]
          (swap-vals! state
                      #(case (first %)
                         :start [:async (partial run r) (partial run e)]
                         :resolved [:completed]
                         :raised [:completed]
                         %))]
      (case before
        :resolved (partial r x)
        :raised (partial e x)
        nil))))

(defn ^:no-doc run-async
  [f resolve raise]
  (let [run (bound-fn trampoline)]
    (run f resolve raise)
    nil))

(defn await
  "Awaits the asynchronous execution of continuation-passing style function
   cps-fn, applying it to args and two extra callback functions: resolve and
   raise. cps-fn is expected to eventually either call resolve with the result,
   call raise with the exception or just throw in the calling thread. The
   return value of cps-fn is ignored. Effectively returns the value passed to
   resolve or throws the exception passed to raise (or thrown) but does not
   block the calling tread.

   Must be called in an asynchronous function. Note that any nested functions
   defined with fn, letfn, reify or deftype are considered outside of
   asynchronous scope."
  [cps-fn & args]
  (throw (new IllegalStateException
              "await called outside of asynchronous scope")))

(def ^:no-doc terminators
  {`await `do-await})

(defmacro afn
  "Defines an asynchronous function. Declared arguments are extended with two
   continuation arguments of &resolve and &raise and these continuations will
   be called with the function result or any exception thrown respectively.

   Executes in the calling thread up until the first await clause. Execution is
   then resumed in the thread the awaited function invokes its continuation in.
   This may still be the calling thread.

   JVM monitor operations (monitor-enter, monitor-exit and locking macro) are
   not supported in the body and their use will lead to concurrency bugs.
   Currently there is no warning when this is the case.

   Only one arity is allowed."
  {:arglists '([name? [params*] body])}
  [& args]
  (let [[a & [b & cs :as bs]] args
        [name args body]
        (if (symbol? a)
          [a b cs] [nil a bs])
        arg-names (map #(if (symbol? %) % (gensym)) args)]
   `(fn ~@(when name [name]) [~@arg-names ~'&resolve ~'&raise]
      (run-async (coroutine ~terminators
                            (loop [~@(interleave args arg-names)] ~@body))
                 ~'&resolve ~'&raise))))

(def
 ^{:macro true
   :deprecated "0.1.9"
   :doc "Deprecated - renamed to afn."}
  fn-async
  #'afn)

(defmacro
 ^{:deprecated "0.1.12"
   :doc "Deprecated - use ((afn [] body*) resolve raise)."}
  async
  [resolve raise & body]
 `((afn [] ~@body) ~resolve ~raise))

(defmacro defn-async
  "Like defn, but the function defined is asynchronous (see afn)."
  {:arglists '([name doc-string? attr-map? [params*] body])}
  [name & args]
  (let [[a & [b & [c & ds :as cs] :as bs]] args
        [doc attrs args body]
        (if (string? a)
          (if (map? b) [a b c ds] [a nil b cs])
          (if (map? a) [nil a b cs] [nil nil a bs]))
        arglists `'([~@args ~'&resolve ~'&raise])
        attrs (update attrs :arglists #(or % arglists))
        arg-names (map #(if (symbol? %) % (gensym)) args)]
   `(defn ~name ~@(when doc [doc]) ~attrs [~@arg-names ~'&resolve ~'&raise]
      (run-async (coroutine ~terminators
                            (loop [~@(interleave args arg-names)] ~@body))
                 ~'&resolve ~'&raise))))

(defn ^:no-doc either-promise
  [cps-fn & args]
  (let [p (promise)
        f (apply partial cps-fn args)
        r #(deliver p [%])
        e #(deliver p [nil %])]
    (try (f r e)
      (catch Throwable t (e t)))
    p))

(defn await!
  "Like await but blocks the calling thread. Do not use inside an asynchronous
   function."
  [cps-fn & args]
  (let [[v t] @(apply either-promise cps-fn args)]
    (if t
      (throw t)
      v)))

(defn ^:no-doc blocking*
  [f]
  (fn [r e]
    (future
      (let [[v t] (try [(f)]
                    (catch Throwable t [nil t]))]
        (if t
          (e t)
          (r v))))))

(defmacro blocking
  "Returns a CPS function that will execute the body in a future block and
   trigger its continuation within that future. Use to avoid long-running
   IO from blocking the caller's thread.
   Use: (await (blocking (slurp \"a-very-long-file.txt\")))"
  [& body]
 `(blocking* (fn [] ~@body)))
