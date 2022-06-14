(ns ^:no-doc await-cps.impl
  (:refer-clojure :exclude [await bound-fn]))

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

(defn do-await
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
          safe-r #(try (r %) (catch Throwable t (e t)))
          other-thread-r #(run safe-r %)
          other-thread-e #(run e %)
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

(defn run-async
  [f resolve raise]
  (let [run (bound-fn trampoline)]
    (run f resolve raise)
    nil))