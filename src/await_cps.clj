(ns await-cps
  "async/await for continuation-passing style (CPS) functions.

   This library delivers async/await expressions for use with asynchronous
   functions that take resolve and raise callbacks as the last parameters
   (e.g. ring, clj-http...)."
  (:refer-clojure :exclude [await])
  (:require [await-cps.ioc :refer [invert]]))

(defmacro ^:no-doc with-binding-frame [frame & body]
 `(let [original-frame# (clojure.lang.Var/getThreadBindingFrame)]
    (clojure.lang.Var/resetThreadBindingFrame ~frame)
    (try
     ~@body
      (finally
        (clojure.lang.Var/resetThreadBindingFrame original-frame#)))))

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
    (let [call-site-frame (clojure.lang.Var/getThreadBindingFrame)
          safe-r #(try (r %) (catch Throwable t (e t)))
          other-thread-r #(with-binding-frame call-site-frame
                            (trampoline safe-r %))
          other-thread-e #(with-binding-frame call-site-frame
                            (trampoline e %))
          [[before x]]
          (swap-vals! state
                      #(case (first %)
                         :start [:async other-thread-r other-thread-e]
                         :resolved [:completed]
                         :raised [:completed]
                         %))]
      (case before
        :resolved (partial safe-r x)
        :raised (partial e x)
        nil))))

(defn default-log-raise-exception
  "Default logger for exceptions in raise callbacks prints the message to stdio."
  [^Throwable t]
  (println "Uncaught exception executing raise callback:" (.getMessage t)))

(def ^:dynamic *log-raise-exception*
  "If the raise callback in an async block throws, the exception will be logged
   with this function."
  default-log-raise-exception)

(defn ^:no-doc run-async
  [f resolve raise]
  (let [e #(try (raise %)
             (catch Throwable t
               (fn []
                 (*log-raise-exception* t)
                 (throw t))))
        r #(try (resolve %) (catch Throwable t (e t)))]
    (with-binding-frame (clojure.lang.Var/getThreadBindingFrame)
      (trampoline #(try (f %1 %2) (catch Throwable t (e t))) r e)))
  nil)

(defn await
  "Awaits asynchronous execution of continuation-passing style function cps-fn,
   applying it to args plus two extra callback functions: resolve and raise.
   Effectively returns the value passed to resolve or throws the exception
   passed to raise. Must be called in an asynchronous function."
  [cps-fn & args]
  (throw (new IllegalStateException "await called outside asynchronous context")))

(def ^:no-doc terminators
  {`await `do-await})

(defmacro async
  "Like ((fn-async [] body*) resolve raise)."
  [resolve raise & body]
  (let [r (gensym)
        e (gensym)]
   `(run-async (fn [~r ~e]
                ~(invert {:r r :e e :terminators terminators :env &env}
                         `(do ~@body)))
               ~resolve ~raise)))

(defmacro afn
  "Defines an asynchronous function. Declared parameters are extended with two
   continuation arguments of &resolve and &raise and these continuations will
   be called with the function result or any exception thrown respectively.

   Executes in the calling thread up until the first await clause. Execution is
   then resumed in the thread awaited function invokes its continuation in.
   This may still be the calling thread.

   Only one arity is allowed."
  {:arglists '([name? [params*] body])}
  [& args]
  (let [[a & [b & cs :as bs]] args
        [name params body]
        (if (symbol? a)
          [a b cs] [nil a bs])
        param-names (map #(if (symbol? %) % (gensym)) params)]
   `(fn ~@(when name [name]) [~@param-names ~'&resolve ~'&raise]
      (async ~'&resolve ~'&raise (loop [~@(interleave params param-names)] ~@body)))))

(def
 ^{:macro true
   :deprecated "0.1.9"
   :doc "Deprecated - renamed to afn."}
  fn-async
  #'afn)

(defmacro defn-async
  "Same as defn but defines an asynchronous function with extra &resolve and
   &raise continuation params. Only one arity is allowed."
  {:arglists '([name doc-string? attr-map? [params*] body])}
  [name & args]
  (let [[a & [b & [c & ds :as cs] :as bs]] args
        [doc attrs params body]
        (if (string? a)
          (if (map? b) [a b c ds] [a nil b cs])
          (if (map? a) [nil a b cs] [nil nil a bs]))
        attrs (assoc attrs :arglists `'([~@params ~'&resolve ~'&raise]))
        param-names (map #(if (symbol? %) % (gensym)) params)]
   `(defn ~name ~@(when doc [doc]) ~attrs [~@param-names ~'&resolve ~'&raise]
      (async ~'&resolve ~'&raise (loop [~@(interleave params param-names)] ~@body)))))

(defn await!
  "Like await but blocks the calling thread. Do not use inside an asynchronous
   function."
  [f & args]
  (let [result (promise)
        resolve #(deliver result [%])
        raise #(deliver result [nil %])]
    (apply f (concat args [resolve raise]))
    (let [[v t] @result]
      (if t
        (throw t)
        v))))
