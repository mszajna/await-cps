(ns await-cps-test
  (:refer-clojure :exclude [await deref])
  (:require [clojure.test :refer :all]
            [await-cps :refer [async await deref]]))

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
  (are [a b] (= a (deref (async b)))
    1 1
    1 (await successful-async 1)

    1 (try (await failing-async) (catch clojure.lang.ExceptionInfo e 1))
    1 (try (await immed-failing-async) (catch clojure.lang.ExceptionInfo e 1))

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
        fail (fn [n r e] (eff n) (e (ex-info "err" {})))
        succ (fn [n v r e] (eff n) (r v))
        effects (fn [as] (reset! efs []) (try (deref as) (catch Throwable t nil)) @efs)]
    (are [a b] (= a (effects (async b)))
      ["a"] (eff "a")
      ["a"] (let [x (await fail "a") z (eff "b")] (eff "c"))
      ["a" "c"] (if (await succ "a" false) (eff "b") (eff "c"))
      ["a" "c"] (if (await succ "a" false) (eff "b") (eff "c"))
    )))
