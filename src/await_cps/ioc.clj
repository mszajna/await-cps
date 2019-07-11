(ns ^:no-doc await-cps.ioc)

(defn var-name [sym]
  (when-let [v (and (symbol? sym) (resolve sym))]
    (let [nm (:name (meta v))
          nsp (.getName ^clojure.lang.Namespace (:ns (meta v)))]
      (symbol (name nsp) (name nm)))))

(defn has-terminal-symbols?
 [form {:keys [terminal-symbols recur-target] :as ctx}]
  (let [sym (when (seq? form) (first form))]
    (cond (contains? terminal-symbols (var-name sym)) true
          (and recur-target (= 'recur sym)) true
          (= 'loop sym) (some #(has-terminal-symbols? % (dissoc ctx :recur-target)) (rest form))
          (coll? form) (some #(has-terminal-symbols? % ctx) form)
          :else false)))

(defn can-inline?
  [form]
  (or (not (coll? form)) ; inline non-collection literals and symbols
      ; can't inline sets or maps as they throw 'Duplicate key'
      ; but inlining vectors is fine
      (and (vector? form) (every? can-inline? form))))

(declare invert)

(defn resolve-sequentially [ctx coll then]
  (let [[syncs [asn & others]] (split-with #(not (has-terminal-symbols? % ctx)) coll)]
    (if asn
      (let [syncs (map #(if (can-inline? %) [%] [(gensym) %]) syncs)
            sync-bindings (->> syncs (filter second) (mapcat identity))
            async-binding (with-meta (gensym) (meta asn))
            cont (gensym "cont")]
       `(let [~@sync-bindings]
          (letfn [(~cont [~async-binding]
                   ~(resolve-sequentially
                      (dissoc ctx :sync-recur?) others
                      #(then `[~@(map first syncs) ~async-binding ~@%])))]
           ~(invert (assoc ctx :r cont) asn))))
      (then coll))))

(defn invert
  [{:keys [r             ; symbol of continuation function (resolve)
           e             ; symbol of error handling function (raise)
           sync-recur?   ; indicates when synchronous recur is possible
           recur-target  ; symbol of asynchronous recur function if any
           terminal-symbols]
    :as ctx}
   form]
  (let [form (macroexpand form)
        [head & tail] (when (seq? form) form)]
    (cond
      (not (has-terminal-symbols? form ctx))
     `(~r ~form)

      (and (seq? form) (special-symbol? head))
      (case head

        (quote var fn* def deftype* reify* clojure.core/import*)
       `(~r ~form)

        if
        (let [[con left right & unexpected-others] tail
              cont (gensym "cont")]
          (if (has-terminal-symbols? con ctx)
            (let [ctx' (dissoc ctx :sync-recur?)]
             `(letfn [(~cont [con#] (if con# ~(invert ctx' left)
                                             ~(invert ctx' right)
                                             ~@unexpected-others))]
               ~(invert (assoc ctx :r cont) con)))
           `(if ~con ~(invert ctx left) ~(invert ctx right))))

        case*
        (let [[ge shift mask default imap & args] tail
              imap (reduce-kv #(assoc %1 %2 (update %3 1 (fn [v] (invert ctx v))))
                              {} imap)]
          `(case* ~ge ~shift ~mask ~(invert ctx default) ~imap ~@args))

        let*
        (let [[syncs [[sym asn] & others]] (->> tail first (partition 2)
                                                (split-with #(not (has-terminal-symbols? % ctx))))
              cont (gensym "cont")]
         `(let* [~@(mapcat identity syncs)]
           ~(if asn
             `(letfn [(~cont [~sym]
                       ~(invert (dissoc ctx :sync-recur?)
                               `(let* [~@(mapcat identity others)]
                                  ~@(rest tail))))]
               ~(invert (assoc ctx :r cont) asn))
              (invert ctx `(do ~@(rest tail))))))

        letfn*
       `(letfn* ~(first tail) ~(invert ctx `(do ~@(rest tail))))

        do
        (let [[syncs [asn & others]] (split-with #(not (has-terminal-symbols? % ctx)) tail)
              cont (gensym "cont")]
          (if asn
           `(do ~@syncs
                ~(if others
                  `(letfn [(~cont [_#] ~(invert (dissoc ctx :sync-recur?)
                                               `(do ~@others)))]
                    ~(invert (assoc ctx :r cont) asn))
                   (invert ctx asn)))
           `(~r ~form)))

        loop*
        (let [[binds & body] tail
              bind-names (->> binds (partition 2) (map first))]
          (cond
            (has-terminal-symbols? binds ctx)
            (invert ctx `(let [~@binds]
                           (loop [~@(interleave bind-names bind-names)]
                            ~@body)))

            (has-terminal-symbols? body (dissoc ctx :recur-target))
            (let [recur-target (gensym "recur")]
             `(letfn [(~recur-target [~@bind-names]
                        (loop [~@(interleave bind-names bind-names)]
                         ~(invert (assoc ctx :sync-recur? true
                                             :recur-target recur-target)
                                 `(do ~@body))))]
                (let [~@binds] (~recur-target ~@bind-names))))

            :else `(~r ~form)))

        recur
        (cond
          (and sync-recur? (not (has-terminal-symbols? form (dissoc ctx :recur-target))))
          form

          recur-target
          (resolve-sequentially ctx tail (fn [args] `(~recur-target ~@args)))

          :else (throw (ex-info "Can't recur outside loop" {:form form})))

        try
        (let [catch-or-finally? #(and (seq? %) (#{'catch 'finally} (first %)))
              [body cfs] (split-with #(not (catch-or-finally? %)) tail)
              [catches finally] (if (->> cfs last first (= 'finally))
                                  [(drop-last cfs) (rest (last cfs))]
                                  [cfs])
              fin-do (gensym "fin-do")
              fin (gensym "finally")
              finThrow (gensym "finallyThrow")
              cat (gensym "catch")
              v (gensym)]
         `(letfn [(~fin-do [~v]
                    (try ~(invert ctx `(do ~@finally (~v)))
                      (catch Throwable t# (~e t#))))
                  (~fin [v#] (~fin-do (constantly v#)))
                  (~finThrow [t#] (~fin-do #(throw t#)))
                  (~cat [t#]
                    (try
                      (try (throw t#)
                       ~@(map (fn [[sym cls bnd & body]]
                               `(~sym ~cls ~bnd
                                       ~(invert (assoc ctx :r fin :e finThrow)
                                               `(do ~@body))))
                              catches))
                      (catch Throwable t# (~finThrow t#))))]
            (try ~(invert (assoc ctx :r fin :e cat) `(do ~@body))
              (catch Throwable t# (~cat t#)))))

        throw
        (resolve-sequentially ctx tail (fn [args] `(~r (throw ~@args))))

        new
        (let [[cls & args] tail]
          (resolve-sequentially ctx args (fn [args] `(~r (new ~cls ~@args)))))

        .
        (let [[subject method & args] tail]
          (if (symbol? subject)
            (resolve-sequentially ctx args
                                  (fn [args] `(~r (. ~subject ~method ~@args))))
            (resolve-sequentially ctx `[~subject ~@args]
                                  (fn [[subject & args]]
                                    `(~r (. ~subject ~method ~@args))))))

        set!
        (let [[subject & args] tail
              [_ object & field-args] (when (and (seq? subject)
                                                 (= '. (first subject)))
                                        subject)]
          (if (and object (has-terminal-symbols? object ctx))
            (resolve-sequentially ctx [object args]
              (fn [[object args]] `(~r (set! (. ~object ~@field-args) ~@args))))
            (resolve-sequentially ctx args
              (fn [args] `(~r (set! ~subject ~@args))))))

        (throw (ex-info (str "Unsupported special symbol [" head "]")
                        {:unknown-special-form head :form form})))

      (contains? terminal-symbols (var-name head))
      (let [handler (terminal-symbols (var-name head))]
        (resolve-sequentially ctx (rest form) (fn [args] `(~handler ~r ~e ~@args))))

      (seq? form)
      (resolve-sequentially ctx form (fn [form] `(~r ~(seq form))))

      (vector? form)
      (resolve-sequentially ctx form (fn [form] `(~r ~(with-meta (vec form) (meta form)))))

      (set? form)
      (resolve-sequentially ctx form (fn [form] `(~r ~(with-meta (set form) (meta form)))))

      (map? form)
      (resolve-sequentially ctx (mapcat identity form)
        (fn [form] `(~r ~(with-meta (->> form (partition 2) (map vec) (into {})) (meta form)))))

      :else (throw (ex-info (str "Unsupported form [" form "]")
                            {:form form})))))
