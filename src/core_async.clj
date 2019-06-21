; (ns core-async
;   (:refer-clojure :exclude [await])
;   (:require [clojure.core.async.impl.ioc-macros :as ioc]))

; (defn await
;   [f & args]
;   (throw (new IllegalStateException "await called outside async block")))

; (defn run-state-machine-wrapped
;   {:no-doc true}
;   [state]
;   (try
;     (ioc/run-state-machine state)
;     nil
;     (catch Throwable ex
;       (let [[resolve raise] (ioc/aget-object state ioc/USER-START-IDX)]
;         (raise ex)
;         nil))))

; (defn do-take
;   {:no-doc true}
;   [state blk f & args]
;   (apply f
;     (concat args
;             [(fn [v]
;               (ioc/aset-all! state ioc/VALUE-IDX v ioc/STATE-IDX blk)
;               (run-state-machine-wrapped state))
;             (fn [e]
;               (if-let [excframes (seq (ioc/aget-object state ioc/EXCEPTION-FRAMES))]
;                 (do
;                   (ioc/aset-all! state
;                                   ioc/VALUE-IDX e
;                                   ioc/STATE-IDX (first excframes)
;                                   ioc/EXCEPTION-FRAMES (rest excframes))
;                   (run-state-machine-wrapped state))
;                 (let [[resolve raise] (ioc/aget-object state ioc/USER-START-IDX)]
;                   (raise e))))])))

; (defn do-return
;   {:no-doc true}
;   [state value]
;   (let [[resolve raise] (ioc/aget-object state ioc/USER-START-IDX)]
;     (if-let [exception (ioc/aget-object state ioc/CURRENT-EXCEPTION)]
;       (raise exception)
;       (resolve value))))

; (def ^:no-doc async-terminators
;   {`await `do-take
;   :Return `do-return})

; (defmacro async
;   "Executes the body eventually calling resolve with the result if successful
;   or reject if failed with exception. If the body contains await clauses
;   the execution will not block the calling thread."
;   [resolve raise & body]
;   (let [crossing-env (zipmap (keys &env) (repeatedly gensym))]
;   `(let [bindings# (clojure.lang.Var/getThreadBindingFrame)
;           ~@(mapcat (fn [[l sym]] [sym `(^:once fn* [] ~(vary-meta l dissoc :tag))]) crossing-env)
;           f# ~(ioc/state-machine `(do ~@body) 1
;                                 [crossing-env &env]
;                                 async-terminators)
;           state# (ioc/aset-all! (f#)
;                                 ioc/USER-START-IDX [~resolve ~raise]
;                                 ioc/BINDINGS-IDX bindings#)]
;       (run-state-machine-wrapped state#))))
