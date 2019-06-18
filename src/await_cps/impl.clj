(ns ^:no-doc await-cps.impl)

(defn full-name [v]
  (when-let [v (and (symbol? v) (resolve v))]
    (let [nm (:name (meta v))
          nsp (.getName ^clojure.lang.Namespace (:ns (meta v)))]
      (symbol (name nsp) (name nm)))))

(defn has-async?
 ([form] (has-async? form false))
 ([form include-recurs?]
  (let [f (when (seq? form) (first form))]
    (cond (= 'await-cps/await (full-name f)) true
          (and include-recurs? (= 'recur f)) true
          (= 'loop f) (some #(has-async? % false) (rest f))
          (coll? form) (some #(has-async? % include-recurs?) form)
          :else false))))

(declare async*)

(defn resolve-all [ctx coll then]
  (let [[syncs [asn & others]] (split-with #(not (has-async? %)) coll)]
    (if asn
      (let [sync-bindings (repeatedly (count syncs) #(gensym))
            async-binding (gensym)
            cont (gensym "cont")]
        ; TODO: some things don't need rebinding (eg. local symbols in &env and literals)
       `(let [~@(interleave sync-bindings syncs)]
          (letfn [(~cont [~async-binding]
                   ~(resolve-all (dissoc ctx :sync-recur?)
                                 others
                                 (fn [bnds] (then `[~@sync-bindings ~async-binding ~@bnds]))))]
           ~(async* (assoc ctx :r cont) asn))))
      (then coll))))

(defn async*
  [{:keys [r             ; symbol of continuation function
           e             ; symbol of error handling function
           sync-recur?   ; indicates when synchronous recur is possible
           recur-target] ; symbol of asynchronous recur function if any
    :as ctx}
   form]
  (let [form (macroexpand form)
        [head & tail] (when (seq? form) form)]
    (cond
      (not (has-async? form (:recur-target ctx)))
     `(~r ~form)

      (and (seq? form) (special-symbol? head))
      (case head

        (quote var fn* def deftype* reify* clojure.core/import*)
       `(~r ~form)

        if
        (let [[con left right] tail
              cont (gensym "cont")]
          (if (has-async? con)
            (let [ctx' (dissoc ctx :sync-recur?)]
             `(letfn [(~cont [con#] (if con# ~(async* ctx' left)
                                             ~(async* ctx' right)))]
               ~(async* (assoc ctx :r cont) con)))
           `(if ~con ~(async* ctx left) ~(async* ctx right))))

        case*
        (let [[ge shift mask default imap & args] tail
              imap (reduce-kv #(assoc %1 %2 (update %3 1 (fn [v] (async* ctx v))))
                              {} imap)]
          `(case* ~ge ~shift ~mask ~(async* ctx default) ~imap ~@args))

        let*
        (let [[syncs [[sym asn] & others]] (->> tail first (partition 2)
                                                (split-with #(not (has-async? %))))
              cont (gensym "cont")]
         `(let* [~@(mapcat identity syncs)]
           ~(if asn
             `(letfn [(~cont [~sym]
                       ~(async* (dissoc ctx :sync-recur?)
                               `(let* [~@(mapcat identity others)]
                                  ~@(rest tail))))]
               ~(async* (assoc ctx :r cont) asn))
              (async* ctx `(do ~@(rest tail))))))

        letfn*
       `(letfn* [~@(first tail)] ~(async* ctx `(do ~@(rest tail))))

        do
        (let [[syncs [asn & others]] (split-with #(not (has-async? % (:recur-target ctx))) tail)
              cont (gensym "cont")]
          (if asn
           `(do ~@syncs
                ~(if others
                  `(letfn [(~cont [_#] ~(async* (dissoc ctx :sync-recur?)
                                               `(do ~@others)))]
                    ~(async* (assoc ctx :r cont) asn))
                   (async* ctx asn)))
           `(~r ~form)))

        loop*
        (let [[binds & body] tail
              bind-names (->> binds (partition 2) (map first))]
          (cond
            (has-async? binds)
            (async* ctx `(let [~@binds]
                           (loop [~@(interleave bind-names bind-names)]
                            ~@body)))

            (has-async? body false)
            (let [recur-target (gensym "recur")]
             `(letfn [(~recur-target [~@bind-names]
                        (loop [~@(interleave bind-names bind-names)]
                         ~(async* (assoc ctx :sync-recur? true
                                             :recur-target recur-target)
                                 `(do ~@body))))]
                (let [~@binds] (~recur-target ~@bind-names))))

            :else `(~r ~form)))

        recur
        (cond
          (and sync-recur? (not (has-async? form)))
          form

          recur-target
          (resolve-all ctx tail (fn [args] `(~recur-target ~@args)))

          :else (throw (new IllegalStateException "recur outside of loop")))

        try
        (let [catch-or-finally? #(and (seq? %) (#{'catch 'finally} (first %)))
              [body cfs] (split-with #(not (catch-or-finally? %)) tail)
              [catches finally] (if (->> cfs last first (= 'finally))
                                  [(map rest (drop-last cfs)) (rest (last cfs))]
                                  [(map rest cfs)])
              fin (gensym "finally")
              finThrow (gensym "finallyThrow")
              cat (gensym "catch")
              v (gensym)]
         `(letfn [(~fin [~v]
                    (future (try ~(async* ctx `(do ~@finally ~v))
                              (catch Throwable t# (~e t#)))))
                  (~finThrow [~v]
                    (future (try ~(async* ctx `(do ~@finally (throw ~v)))
                              (catch Throwable t# (~e t#)))))
                  (~cat [t#]
                    (try
                      (try (throw t#)
                       ~@(map (fn [[cls bnd & body]]
                               `(catch ~cls ~bnd
                                       ~(async* (assoc ctx :r fin :e finThrow)
                                               `(do ~@body))))
                              catches))
                      (catch Throwable t# (~finThrow t#))))]
            (try ~(async* (assoc ctx :r fin :e cat) `(do ~@body))
              (catch Throwable t# (~cat t#)))))

        (monitor-enter monitor-exit throw)
        (resolve-all ctx tail (fn [args] `(~r (~head ~@args))))

        new
        (let [[cls & args] tail]
          (resolve-all ctx args
                       (fn [args] `(~r (new ~cls ~@args)))))

        .
        (let [[subject method & args] tail]
          (if (symbol? subject)
            (resolve-all ctx args
                         (fn [args] `(~r (. ~subject ~method ~@args))))
            (resolve-all ctx `[~subject ~@args]
                         (fn [[subject & args]]
                          `(~r (. ~subject ~method ~@args))))))

        set!
        (let [[subject value] tail
              [object field] (when (and (seq? subject) (= '. (first subject)))
                               subject)]
          (if (and object (has-async? object))
            (resolve-all ctx [object value]
                         (fn [[obj v]] `(set! (. ~obj ~field) ~v)))
            (resolve-all ctx value (fn [v] `(set! ~subject ~v)))))

        (throw (ex-info (str "Unsupported special symbol [" head "]")
                        {:unknown-special-form head :form form})))

      (and (seq? form) (symbol? head) (= 'await-cps/await (full-name head)))
      (resolve-all ctx (rest form)
                  (fn [args] `(~@args (fn [v#] (try (~r v#)
                                                  (catch Throwable t# (~e t#))))
                                      (fn [t#] (~e t#)))))

      (seq? form)
      (resolve-all ctx form (fn [form] `(~r ~(seq form))))

      (vector? form)
      (resolve-all ctx form (fn [form] `(~r ~(vec form))))

      (set? form)
      (resolve-all ctx form (fn [form] `(~r ~(set form))))

      (map? form)
      (resolve-all ctx (mapcat identity form)
                   (fn [form] `(~r ~(->> form (partition 2) (map vec) (into {})))))
      :else (throw (ex-info (str "Unsupported form [" form "]")
                            {:form form})))))
