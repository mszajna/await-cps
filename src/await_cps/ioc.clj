(ns ^:no-doc await-cps.ioc)

(defn var-name [env sym]
  (when-let [v (and (symbol? sym) (resolve env sym))]
    (let [nm (:name (meta v))
          nsp (.getName ^clojure.lang.Namespace (:ns (meta v)))]
      (symbol (name nsp) (name nm)))))

(defn has-terminators?
 [form {:keys [terminators recur-target env] :as ctx}]
  (let [sym (when (seq? form) (first form))]
    (cond (contains? terminators (var-name env sym)) true
          (and recur-target (= 'recur sym)) true
          (= 'loop* sym) (some #(has-terminators? % (dissoc ctx :recur-target)) (rest form))
          (coll? form) (some #(has-terminators? % ctx) form)
          :else false)))

(defn can-inline?
  [form]
  (or (not (coll? form)) ; inline non-collection literals and symbols
      ; can't inline sets or maps as they throw 'Duplicate key'
      ; but inlining vectors is fine
      (and (vector? form) (every? can-inline? form))))

(declare invert)

(defn resolve-sequentially [ctx coll then]
  (let [[syncs [asn & others]] (split-with #(not (has-terminators? % ctx)) coll)]
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

(defn add-env-syms [ctx syms]
  ;; This adds mappings to "true" into the environment map. This doesn't quite
  ;; match what the Clojure compiler does, but I think it's already recommended
  ;; that macros don't depend on the values in the &env map.
  (update ctx :env (fnil into {}) (map (fn [sym] [sym true])) syms))

(defn invert
  [{:keys [r             ; symbol of continuation function (resolve)
           e             ; symbol of error handling function (raise)
           sync-recur?   ; indicates when synchronous recur is possible
           recur-target  ; symbol of asynchronous recur function if any
           terminators   ; map of symbols that break flow to symbols of handlers
           env]          ; the current macroexpansion environment
    :as ctx}
   form]
  (let [[head & tail] (when (seq? form) form)
        resolved (when (symbol? head) (resolve env head))]
    (cond
      (not (has-terminators? form ctx))
     `(~r ~form)

      (and resolved (.isMacro resolved))
      (recur ctx (apply resolved form env tail))

      (special-symbol? head)
      (case head

        (quote var fn* def deftype* reify* clojure.core/import*)
       `(~r ~form)

        if
        (let [[con left right & unexpected-others] tail
              cont (gensym "cont")]
          (if (has-terminators? con ctx)
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
                                                (split-with #(not (has-terminators? % ctx))))
              cont (gensym "cont")
              updated-ctx (add-env-syms ctx (map first syncs))]
         `(let* [~@(mapcat identity syncs)]
           ~(if asn
             `(letfn [(~cont [~sym]
                       ~(invert (add-env-syms (dissoc updated-ctx :sync-recur?) [sym])
                               `(let* [~@(mapcat identity others)]
                                  ~@(rest tail))))]
               ~(invert (assoc updated-ctx :r cont) asn))
              (invert updated-ctx `(do ~@(rest tail))))))

        letfn*
       `(letfn* ~(first tail)
         ~(invert (add-env-syms ctx (->> tail first (partition 2) (map first)))
                 `(do ~@(rest tail))))

        do
        (let [[syncs [asn & others]] (split-with #(not (has-terminators? % ctx)) tail)
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
            (has-terminators? binds ctx)
            (invert ctx `(let [~@binds]
                           (loop [~@(interleave bind-names bind-names)]
                            ~@body)))

            (has-terminators? body (dissoc ctx :recur-target))
            (let [recur-target (gensym "recur")
                  updated-ctx (add-env-syms ctx bind-names)]
             `(letfn [(~recur-target [~@bind-names]
                        (loop [~@(interleave bind-names bind-names)]
                         ~(invert (assoc updated-ctx
                                         :sync-recur? true
                                         :recur-target recur-target)
                                 `(do ~@body))))]
                (let [~@binds] (~recur-target ~@bind-names))))

            :else `(~r ~form)))

        recur
        (cond
          (and sync-recur? (not (has-terminators? form (dissoc ctx :recur-target))))
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
              fin-do (gensym "finally-do")
              fin (gensym "finally")
              fin-throw (gensym "finally-throw")
              cat (gensym "catch")
              v (gensym) t (gensym)]
         `(letfn [(~fin-do [~v ~t]
                    (try ~(invert ctx `(do ~@finally (if ~t (throw ~t) ~v)))
                      (catch Throwable t# (~e t#))))
                  (~fin [v#] (~fin-do v# nil))
                  (~fin-throw [t#] (~fin-do nil t#))
                  (~cat [t#]
                    (try
                      (try (throw t#)
                       ~@(map (fn [[sym cls bnd & body]]
                               `(~sym ~cls ~bnd
                                       ~(invert (assoc (add-env-syms ctx [bnd]) :r fin :e fin-throw)
                                               `(do ~@body))))
                              catches))
                      (catch Throwable t# (~fin-do nil t#))))]
            (try ~(invert (assoc ctx :r fin :e cat) `(do ~@body))
              (catch Throwable t# (~cat t#)))))

        throw
        (resolve-sequentially ctx tail (fn [args] `(throw ~@args)))

        new
        (let [[cls & args] tail]
          (resolve-sequentially ctx args (fn [args] `(~r (new ~cls ~@args)))))

        .
        (let [[subject second] tail
              [method & args] (if (seq? second) second (rest tail))]
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
          (if (and object (has-terminators? object ctx))
            (resolve-sequentially ctx [object args]
              (fn [[object args]] `(~r (set! (. ~object ~@field-args) ~@args))))
            (resolve-sequentially ctx args
              (fn [args] `(~r (set! ~subject ~@args))))))

        (throw (ex-info (str "Unsupported special symbol [" head "]")
                        {:unknown-special-form head :form form})))

      (contains? terminators (var-name env head))
      (let [handler (terminators (var-name env head))]
        (resolve-sequentially ctx (rest form) (fn [args] `(~handler ~r ~e ~@args))))

      (seq? form)
      (resolve-sequentially ctx form (fn [form] `(~r ~(seq form))))

      (vector? form)
      (resolve-sequentially ctx form (fn [form] `(~r ~(with-meta (vec form) (meta form)))))

      (set? form)
      (resolve-sequentially ctx form (fn [form] `(~r ~(with-meta (set form) (meta form)))))

      (map? form)
      (resolve-sequentially ctx (mapcat identity form)
                            (fn [form] `(~r ~(with-meta (apply array-map form) (meta form)))))

      :else (throw (ex-info (str "Unsupported form [" form "]")
                            {:form form})))))
