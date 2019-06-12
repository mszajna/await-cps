(ns await-cps-test
  (:refer-clojure :exclude [await])
  (:require [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]
            [await-cps :refer [async await]]))

; The goal of this library is to enable writing CPS (continuation-passing style)
; code preserving the flow of synchronous code. It achieves that elevating
; the CPS calls deep in a form all the way up

(def side-effects (atom nil))
(def timeout 50)

(defn run-async [form]
  (reset! side-effects nil)
  (let [form `(let [result# (promise)]
                (async #(deliver result# %) #(deliver result# [:exception (.getMessage %)]) ~form)
                result#)
        result (try (deref (eval form) timeout :timeout) (catch Throwable t [:exception (.getMessage t)]))]
    [@side-effects result]))

(defn run-sync [form]
  (reset! side-effects nil)
  (with-redefs [await #(apply % %&)]
    (let [result (try (eval form) (catch Throwable t [:exception (.getMessage t)]))]
      [@side-effects result])))

(defn sync-equiv? [form]
  (= (run-sync form) (run-async form)))

(defn explain [form]
  (clojure.pprint/pprint
    {:compiled (await-cps/async* 'r 'e form)
     :sync (run-sync form)
     :async (run-async form)}))

(def exc
  (doto (new clojure.lang.ExceptionInfo "exc" {})
        (.setStackTrace (make-array StackTraceElement 0))))

(defn effect [s] (swap! side-effects concat [s]))

(defn return
  ([s v] (effect s) v)
  ([s v r e] (effect s) (future (r v))))
(defn fail
  ([s t] (effect s) (throw t))
  ([s t r e] (effect s) (future (e t))))

(defn returning [v]
  (gen/let [_ (gen/return nil)]
    (let [s (str (gensym))] ; Function calls need identity
      (gen/elements [`(return ~s ~v) `(await return ~s ~v)
                     `(fail ~s exc) `(await fail ~s exc)]))))

(def any-call (gen/let [v gen/keyword]
                (returning v)))

(defn sync-equiv [gen-form]
  (for-all [form gen-form]
    (= (run-sync form) (run-async form))))

(defspec call-form 10
  (sync-equiv any-call))

(defspec do-form 30
  (sync-equiv (gen/let [calls (gen/vector any-call 0 3)]
                `(do ~@calls))))

(defspec if-form 10
  (sync-equiv (gen/let [b gen/boolean
                        t (returning b)
                        alts (gen/vector any-call 1 2)]
                `(if ~t ~@alts))))

(def bindable (gen/fmap symbol gen/keyword))

(defspec let-form 20
  (sync-equiv (gen/let [a-sym bindable
                        a-val any-call
                        b-sym bindable
                        b-val (returning a-sym)
                        calls (gen/vector (gen/one-of [(returning a-sym) (returning b-sym)]) 0 3)]
                `(let [~a-sym ~a-val
                       ~b-sym ~b-val] ~@calls))))

(defspec try-form 30
  (let [gen-catch (gen/let [cls (gen/elements ['clojure.lang.ExceptionInfo 'UnsupportedOperationException])
                            bnd bindable
                            body (gen/vector any-call 1 2)]
                    `(catch ~cls ~bnd ~@body))
        gen-finally (gen/let [body (gen/vector any-call 1 2)]
                      `(finally ~@body))]
    (sync-equiv (gen/let [body (gen/vector any-call 1 3)
                          catches (gen/vector gen-catch 0 2)
                          finally (gen/vector gen-finally 0 1)]
                  `(try ~@body ~@catches ~@finally)))))

; Old stuff

; (defn successful-async [v r e]
;   (future (r v)))

; (defn failing-async [r e]
;   (future (e (ex-info "failing" {}))))

; (defn immed-failing-async [r e]
;   (throw (ex-info "immed-failing" {})))

; (def ex (ex-info "an exception" {}))

; (deftest async-await
;   (are [a b] (= a (let [p (promise)]
;                   (try (async #(deliver p %) #(deliver p %) b) (catch Throwable t (deliver p t)))
;                   (deref p 100 :timeout)))
;     1 1
;     1 (await successful-async 1)

;     1 (do (await successful-async 0) 1)
;     ex (do (await successful-async 0) (throw ex))

;     1 (try (await failing-async) (catch clojure.lang.ExceptionInfo e 1))
;     1 (try (await immed-failing-async) (catch clojure.lang.ExceptionInfo e 1))
;     ex (try (await failing-async) (catch Throwable t (throw ex)))

;     1 (if (await successful-async false) 0 1)
;     1 (if true (await successful-async 1) 0)
;     1 (if false 0 (await successful-async 1))

;     1 (case (await successful-async 1) 1 1 2)
;     1 (case 0 0 (await successful-async 1) 2)
;     1 (case 0 2 2 (await successful-async 1))

;     1 (let [x 0 y (await successful-async x) z (inc y)] z)
;     1 (let [a 1] (await successful-async a))
;     1 (let [a 1] (await successful-async 2) (await successful-async 1))

;     1 (loop [a 10] (if (= a 1) a (recur (await successful-async (dec a)))))
;     1 (loop [a 0 b (inc a)] (await successful-async b))

;     1 ((await successful-async inc) 0)
;     [1 2] [(await successful-async 1) 2]
;     #{1 2} #{(await successful-async 1) 2}
;     {1 2 3 4} {(await successful-async 1) 2 3 (await successful-async 4)}
;   ))

; (deftest async-await-side-effects
;   (let [efs (atom nil)
;         eff (fn [n] (swap! efs conj n))
;         fail (fn [n r e] (future (eff n) (e (ex-info "err" {}))))
;         succ (fn [n v r e] (future (eff n) (r v)))]
;     (are [a b] (= a (let [p (promise)]
;                     (reset! efs [])
;                     (try (async #(deliver p %) #(deliver p %) b) (catch Throwable t (deliver p t)))
;                     (deref p 100 :timeout)
;                     @efs))
;       ["a"] (eff "a")
;       ["a" "b"] (await succ "b" (eff "a"))
;       ["a" "b" "c"] (str (eff "a") (await succ "b" nil) (eff "c"))
;       ["a" "b"] (do (eff "a") (await succ "b" nil))
;       ["a" "b"] (do (await succ "a" nil) (eff "b"))
;       ["a"] (let [x (await fail "a") z (eff "b")] (eff "c"))
;       ["a" "c"] (if (await succ "a" false) (eff "b") (eff "c"))
;       ["a" "c"] (if (await succ "a" false) (eff "b") (eff "c"))
;       ["a" "b"] (try (eff "a") (finally (eff "b")))
;       ["b"] (try (throw (ex-info "a" {})) (finally (eff "b")))
;       ["a" "b"] (try (await succ "a" nil) (finally (eff "b")))
;       ["a" "b"] (try (await fail "a") (finally (eff "b")))
;       ["a" "b"] (try (eff "a") (finally (await succ "b" nil)))
;       ["b"] (try (throw (ex-info "a" {})) (finally (await succ "b" nil)))
;       ["a" "b" "c"] (try (await fail "a") (catch Throwable t (eff "b")) (finally (eff "c")))
;       ["a" "b" "c"] (try (await fail "a") (catch Throwable t (eff "b")) (finally (await succ "c" nil)))
;     )))
