(ns ^:no-doc await.impl
  (:refer-clojure :exclude [bound-fn]))

#?(:clj
    (defn bound-fn
      [f]
      (let [bound-frame (clojure.lang.Var/getThreadBindingFrame)]
        (fn [& args]
          (let [call-site-frame (clojure.lang.Var/getThreadBindingFrame)]
            (clojure.lang.Var/resetThreadBindingFrame bound-frame)
            (try
              (apply f args)
              (finally
                (clojure.lang.Var/resetThreadBindingFrame call-site-frame)))))))
   :cljs
    (def bound-fn identity))

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
