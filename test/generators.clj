(ns generators
  (:require [clojure.test.check.generators :as gen]))

(defn call [fn-gen & arg-gens]
  (letfn [(call [args arg-gen & arg-gens]
            (if arg-gen
              (gen/bind arg-gen (fn [arg] (apply call (concat args [arg]) arg-gens)))
              args))]
    (gen/bind fn-gen (fn [f] (apply call (list f) arg-gens)))))

(defn an-if [test-gen alts-gen]
  (gen/let [t test-gen
            alts (gen/vector alts-gen 1 2)]
    `(if ~t ~@alts)))

(defn a-do [body-gen]
  (gen/fmap (fn [f] `(do ~@f)) body-gen))

(defn bindings [syms->bind-gen n]
  (loop [n n binds-gen (gen/return [])]
    (if (<= n 0)
      binds-gen
      (recur (dec n)
             (gen/let [binds binds-gen
                       value (syms->bind-gen (take-nth 2 binds))]
               `[~@binds sym# ~value])))))

(defn a-let [syms->binds-gen syms->body-gen]
  (gen/let [binds (gen/bind (gen/choose 1 2) #(bindings syms->binds-gen %))
            body (syms->body-gen (take-nth 2 binds))]
    `(let ~binds ~@body)))

(def ex-cls (gen/elements ['clojure.lang.ExceptionInfo 'UnsupportedOperationException]))

(defn a-catch [cls-gen sym->body-gen]
  (let [sym (gensym "e")]
    (gen/let [cls cls-gen
              body (sym->body-gen sym)]
      `(catch ~cls ~sym ~@body))))

(defn a-try [body-gen sym->catch-gen fin-gen]
  (gen/let [body body-gen
            catches (gen/vector (a-catch ex-cls sym->catch-gen) 0 2)
            finally (gen/vector (gen/fmap (fn [f] `(finally ~@f)) fin-gen) 0 1)]
    `(try ~@body ~@catches ~@finally)))

; call-gen [syms value-gen] returns code that consumes symbols and returns a value
(defn clojure-code [call-gen val-gen]
  (letfn [(multi [syms ret-gen]
            (gen/let [calls (gen/vector (code syms val-gen) 0 1)
                      ret (code syms ret-gen)] `(~@calls ~ret)))
          (code [syms ret-gen]
            (gen/bind (gen/return nil)
                      (fn [_]
              (gen/frequency  [[100 ret-gen]
                               [20 (call-gen syms (code syms ret-gen))]
                               [10 (an-if (code syms gen/boolean) (code syms ret-gen))]
                               [10 (a-do (multi syms ret-gen))]
                               [10 (a-let #(code (set (concat syms %)) val-gen) #(multi (set (concat syms %)) ret-gen))]
                               [5 (a-try (multi syms ret-gen) #(multi (conj syms %) ret-gen) (multi syms val-gen))]]))))]
    (code #{} val-gen)))
