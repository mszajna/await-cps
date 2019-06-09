(ns await-cps-test
  (:refer-clojure :exclude [await])
  (:require [clojure.test :refer :all]
            [await-cps :refer [async await]]))

; The goal of this library is to enable writing CPS (continuation-passing style)
; code preserving the flow of synchronous code. It achieves that elevating
; the CPS calls deep in a form all the way up

(defn successful-async [v r e]
  (future (r v)))

(defn failing-async [r e]
  (future (e (ex-info "failing" {}))))

(defn immed-failing-async [r e]
  (throw (ex-info "immed-failing" {})))

(deftest async-await
  (are [a b] (= a (let [p (promise)]
                   (async #(deliver p %) #(deliver p %) b)
                   (deref p 100 :timeout)))
    1 1
    1 (await successful-async 1)

    ; 1 (try (await failing-async) (catch clojure.lang.ExceptionInfo e 1))
    ; 1 (try (await immed-failing-async) (catch clojure.lang.ExceptionInfo e 1))

    1 (if (await successful-async false) 0 1)
    1 (if true (await successful-async 1) 0)
    1 (if false 0 (await successful-async 1))

    1 (case (await successful-async 1) 1 1 2)
    1 (case 0 0 (await successful-async 1) 2)
    1 (case 0 2 2 (await successful-async 1))

    1 (let [x 0 y (await successful-async x) z (inc y)] z)
    1 (let [a 1] (await successful-async a))
    1 (let [a 1] (await successful-async 2) (await successful-async 1))

    1 (loop [a 10] (if (= a 1) a (recur (await successful-async (dec a)))))
    1 (loop [a 0 b (inc a)] (await successful-async b))

    1 ((await successful-async inc) 0)
    [1 2] [(await successful-async 1) 2]
    #{1 2} #{(await successful-async 1) 2}
    {1 2 3 4} {(await successful-async 1) 2 3 (await successful-async 4)}
  ))

(deftest async-await-side-effects
  (let [efs (atom nil)
        eff (fn [n] (swap! efs conj n))
        fail (fn [n r e] (future (eff n) (e (ex-info "err" {}))))
        succ (fn [n v r e] (future (eff n) (r v)))]
    (are [a b] (= a (let [p (promise)]
                    (reset! efs [])
                    (async #(deliver p %) #(deliver p %) b)
                    (deref p 100 :timeout)
                    @efs))
      ["a"] (eff "a")
      ["a" "b"] (await succ "b" (eff "a"))
      ["a" "b" "c"] (str (eff "a") (await succ "b" nil) (eff "c"))
      ["a"] (let [x (await fail "a") z (eff "b")] (eff "c"))
      ["a" "c"] (if (await succ "a" false) (eff "b") (eff "c"))
      ["a" "c"] (if (await succ "a" false) (eff "b") (eff "c"))
      ["a" "b"] (try (eff "a") (finally (eff "b")))
      ["b"] (try (throw (ex-info "a" {})) (finally (eff "b")))
      ["a" "b"] (try (await succ "a" nil) (finally (eff "b")))
      ["a" "b"] (try (await fail "a") (finally (eff "b")))
      ["a" "b"] (try (eff "a") (finally (await succ "b" nil)))
      ["b"] (try (throw (ex-info "a" {})) (finally (await succ "b" nil)))
      ["a" "b" "c"] (try (await fail "a") (catch Throwable t (eff "b")) (finally (eff "c")))
    )))
