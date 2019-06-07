(ns await-cps-test)

; The goal of this library is to enable writing CPS (continuation-passing style)
; code preserving the flow of synchronous code. It achieves that elevating
; the CPS calls deep in a form all the way up

; Ideally, synchronous code would map to itself. Transforming a form,
; any subform that doesn't do CPS should be presesrved as well.
; The async2-macro doesn't do it, favouring the simplicity of implementation.
; I did have a go at trying to preserve forms before but got nowhere

(comment
  (when (sync? a)
    (= (transform a) [:sync a]))
  (when (async? a)
    (= (transform a) [:async a]))
  (when (sync? a)
    (= (transform (await a)) [:async a]))
  (when (and (sync? a) (sync? b))
    (= (transform (f a b)) [:sync (f a b)]))
  (when (and (async? a) (sync? b))
    (= (transform (f a b)) [:async (fn [r e] (a (fn [av] (r (f av b))) e))]))
  (when (and (sync? a) (async? b))
    (= (transform (f a b)) [:async (fn [r e] (let [av a] (b (fn [bv] (r (f av bv))) e)))]))
  (when (and (async? a) (async? b))
    (= (transform (f a b)) [:async (fn [r e] (a (fn [av] (b (fn [bv] (r (f av bv))) e)) e))]))

  (do (await a) b) => (a (fn [_] (r b)) e)
  (do a (await c)) => (do a (c (fn [cv] (r cv)) e))
  
  (do a) => (r a)
  (do (await a) & xs) => (~a (fn [_] (do ~@xs)))
  (do (await a)) => (~a (fn [av] (r av)))
  
  (async (loop [a 1] (if (await f a) (recur (inc a)) a))) => ((fn rec [a] (f a (fn [fv] (if fv (rec (inc a)) (r a))) e)) 1)
  
  (async (try (await f) (catch Exception e a))) => (fn [r e] (try (f (fn [fv] (r fv))
                                                                     (fn [ex] (try (throw ex)
                                                                                   (catch Exception e (r a))
                                                                                   (catch Throwable t (e t)))))
                                                               (catch Exception e (r a))
                                                               (catch Throwable t (e t))))
  
  (async [(println "1") (throw ex)]) ??
)
