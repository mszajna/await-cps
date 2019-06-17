(ns await-cps
  (:refer-clojure :exclude [await]))

(defn await
  "Awaits asynchronous execution of continuation-passing style function f,
   applying it to args and two callbacks for success and failure. Will return
   the value passed to success callback or throw the value passed to failure
   callback. Must be called inside async block."
  [f & args]
  (throw (new IllegalStateException "await called outside async block")))

(defn has-async? {:no-doc true}
 ([form] (has-async? form true))
 ([form include-recurs?]
  (let [f (if (and (seq? form) (symbol? (first form))) (first form))]
    (cond (and f (= #'await (resolve f))) true
          (and include-recurs? (= 'recur f)) true
          (= 'loop f) (some #(has-async? % false) (rest f))
          (coll? form) (some #(has-async? % include-recurs?) form)
          :else false))))

(defmacro async* [{:keys [r e sync-loop recur-target] :as ctx} form]
  (let [form (macroexpand form)]
    (letfn [(call [form bnd->form]
              (let [[syncs [asn & others]] (split-with #(not (has-async? %)) form)
                    sync-bindings (repeatedly (count syncs) #(gensym))
                    async-binding (gensym)
                    cont (gensym "cont")]
                (if asn
                 `(let [~@(interleave sync-bindings syncs)] ; TODO: some things don't need rebinding (local symbols in &env and literals)
                    (letfn [(~cont [~async-binding]
                             ~(call others (fn [bnds] (bnd->form `[~@sync-bindings ~async-binding ~@bnds]))))]
                      (async* ~(assoc ctx :r cont) ~asn)))
                 (bnd->form form))))]
      (cond
        (not (has-async? form recur-target)) `(~r ~form)
        (and (seq? form) (special-symbol? (first form)))
        (let [[head & tail] form]
          (case head
            (quote var fn* def deftype* reify* clojure.core/import*) `(~r ~form)
            (monitor-enter monitor-exit throw) (call tail (fn [bnds] `(~r (~head ~@bnds))))
            new (call (rest tail) (fn [bnds] `(~r (new ~(first tail) ~@bnds))))
            .
            (if (symbol? (first tail))
              (call (rest tail) (fn [bnds] `(~r (~head ~(first tail) ~@bnds))))
              (call tail (fn [bnds] `(~r (~head ~@bnds)))))
            set!
            (if (and (seq? (first tail)) (= '. (ffirst tail)))
              (if (symbol? (second (first tail))) ; assignment special form, TODO: only when the symbol is *not* local (see &env) and resolves to a class
                (call [(nnext (first tail)) (rest tail)] (fn [[v bnds]] `(set! (. ~(second (first tail)) ~@v) ~@bnds)))
                (call [(rest (first tail)) (rest tail)] (fn [[v bnds]] `(set! (. ~@v) ~@bnds))))
              (call (rest tail) (fn [bnds] `(set! ~@bnds))))
            if
            (let [[con left right] tail
                  cont (gensym "cont")]
              (if (has-async? con)
                (let [ctx (dissoc ctx :sync-loop)]
                 `(letfn [(~cont [con#] (if con# (async* ~ctx ~left) (async* ~ctx ~right)))]
                    (async* ~(assoc ctx :r cont) ~con)))
               `(if ~con (async* ~ctx ~left) (async* ~ctx ~right))))
            case*
            (let [[ge shift mask default imap & args] tail
                  imap (reduce-kv #(assoc %1 %2 (update %3 1 (fn [v] `(async* ~ctx ~v)))) {} imap)]
              `(case* ~ge ~shift ~mask (async* ~ctx ~default) ~imap ~@args))
            let*
            (let [[syncs [[sym asn] & others]] (->> tail first (partition 2) (split-with #(not (has-async? %))))
                  cont (gensym "cont")]
             `(let* [~@(mapcat identity syncs)]
               ~(if asn
                 `(letfn [(~cont [~sym] (async* ~(dissoc ctx :sync-loop) (let* [~@(mapcat identity others)] ~@(rest tail))))]
                    (async* ~(assoc ctx :r cont) ~asn))
                 `(async* ~ctx (do ~@(rest tail))))))
            letfn* `(letfn* [~@(first tail)] (async* ~ctx (do ~@(rest tail))))
            do
            (let [[syncs [asn & others]] (split-with #(not (has-async? %)) tail)
                  cont (gensym "cont")]
              (if asn
               `(do ~@syncs
                    ~(if others
                      `(letfn [(~cont [_#] (async* ~(dissoc ctx :sync-loop) (do ~@others)))]
                         (async* ~(assoc ctx :r cont) ~asn))
                      `(async* ~ctx ~asn)))
               `(~r ~form)))
            loop*
            (let [[binds & body] tail
                  bind-names (->> binds (partition 2) (map first))]
              (cond
                (has-async? binds)
               `(async* ~ctx (let [~@binds] (loop [~@(interleave bind-names bind-names)] ~@body)))
                (has-async? body)
                (let [recur-target (gensym "recur")]
                 `(letfn [(~recur-target [~@bind-names]
                            (loop [~@(interleave bind-names bind-names)]
                              (async* ~(assoc ctx :sync-loop true :recur-target recur-target) (do ~@body))))]
                    (let [~@binds] (~recur-target ~@bind-names))))
                :else `(~r ~form)))
            recur
            (cond
              (and sync-loop (not (has-async? form false))) form
              recur-target (call tail (fn [args] `(~recur-target ~@args)))
              :else (throw (new IllegalStateException "recur outside of loop")))
            try
            (let [[body cfs] (split-with #(not (and (seq? %) (#{'catch 'finally} (first %)))) tail)
                  [catches finally] (if (->> cfs last first (= 'finally))
                                      [(map rest (drop-last cfs)) (rest (last cfs))]
                                      [(map rest cfs)])
                  fin (gensym "finally") finThrow (gensym "finallyThrow") cat (gensym "catch")]
             `(letfn [(~fin [v#] (future (try (async* ~ctx (do ~@finally v#)) (catch Throwable t# (~e t#)))))
                      (~finThrow [t#] (future (try (async* ~ctx (do ~@finally (throw t#))) (catch Throwable t# (~e t#)))))
                      (~cat [t#] (try (try (throw t#)
                                       ~@(map (fn [[cls bnd & body]] `(catch ~cls ~bnd (async* ~(assoc ctx :r fin :e finThrow) (do ~@body))))
                                              catches))
                                      (catch Throwable t# (~finThrow t#))))]
                (try (async* ~(assoc ctx :r fin :e cat) (do ~@body))
                  (catch Throwable t# (~cat t#)))))
            (throw (ex-info (str "Don't know how to handle special form [" head "]") {:unknown-special-form head}))))
        (and (seq? form) (symbol? (first form)) (= #'await (resolve (first form))))
        (call (rest form) (fn [args] `(~@args (fn [v#] (try (~r v#) (catch Throwable t# (~e t#)))) (fn [t#] (~e t#)))))

        (seq? form) (call form (fn [form] `(~r ~(seq form))))
        (vector? form) (call form (fn [form] `(~r ~(vec form))))
        (set? form) (call form (fn [form] `(~r ~(set form))))
        (map? form) (call (mapcat identity form) (fn [form] `(~r ~(->> form (partition 2) (map vec) (into {})))))
        :else `(~r ~form)))))

(defmacro async
  "Executes the body eventually calling ret with the result if successful.
   Any exceptions will either bubble up normally or will be passed to err.
   If the body contains await clauses the execution will not block the calling
   thread."
  [ret err & body]
 `(let [r# ~ret e# ~err] (async* {:r r# :e e#} (do ~@body)) nil))
