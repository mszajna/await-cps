(ns await-cps
  (:refer-clojure :exclude [await deref])
  (:require [clojure.walk :refer [macroexpand-all]]))

(defn await [f & args]
  (throw (new IllegalStateException "await called outside async block")))

; 'af' or 'async-form' is a function (fn [r e] form) and returns a form
; 'r' and 'e' are functions that given a form return a new form

(defn- map-af [f af]
  (fn [r e] (af (comp r f) e)))

(defn- mapcat-af [f af]
  (fn [r e] (af (fn [form] ((f form) r e)) e)))

(defn- inline-form [form]
  (fn [r e] (r form)))

(defn- sync-form [form]
  (let [x (gensym "x")]
    (fn [r e] `(let [~x ~form] ~(r x)))))

(defn- reduce-do [afs]
  (reduce (fn ([xs x] (fn [r e] (xs (fn [xs-form] `(do ~xs-form ~(x r e))) e)))
              ([] (inline-form nil)))
          afs))

(defn- reduce-call [afs]
  (->> afs
       (map #(mapcat-af (fn [form] (if (coll? form) (sync-form form) (inline-form form))) %))
       (reduce (fn [xs x] (fn [r e] (xs (fn [xs-form] (x (fn [x-form] (r `(~@xs-form ~x-form))) e)) e)))
               (inline-form ()))))

(defn- reduce-binds [binds]
  (reduce (fn [xs [sym x]] (fn [r e] (xs (fn [bound-syms] (x (fn [x-form] `(let [~sym ~x-form] ~(r (conj bound-syms sym)))) e)) e)))
          (inline-form [])
          binds))

(def ^:private ^:dynamic recur-target) ; this is compile-time use only

(defn walk [form]
  (let [call #(->> form (map walk) reduce-call)
        pass (inline-form form)]
    (cond
      (and (seq? form) (special-symbol? (first form)))
      (let [[head & tail] form]
        (case head
          quote pass
          var pass
          fn* pass
          def pass
          deftype* pass
          reify pass
          letfn* pass
          import* pass

          . (call)
          new (call)
          set! (call)
          monitor-enter (call)
          monitor-exit (call)

          do (->> tail (map walk) reduce-do)

          if (let [[test-af & alt-afs] (map walk tail)]
               (fn [r e] (test-af (fn [test-form] `(if ~test-form ~@(map #(% r e) alt-afs))) e)))

          case* (let [[ge shift mask default imap & args] tail
                      default-af (walk default)
                      imap-af (fn [r e] (->> imap
                                             (map (fn [[hash [val form]]] [hash [val ((walk form) r e)]]))
                                             (into {})))]
                  (fn [r e] `(case* ~ge ~shift ~mask ~(default-af r e) ~(imap-af r e) ~@args)))

          let* (let [binds-af (->> tail first (partition 2) (map (fn [[sym form]] [sym (walk form)])) reduce-binds)
                     body-af (->> tail rest (map walk) reduce-do)]
                 (->> binds-af (mapcat-af (fn [_] body-af))))

          loop* (let [bind-names (->> tail first (partition 2) (map first))
                      binds-af (->> tail first (partition 2) (map (fn [[sym form]] [sym (walk form)])) reduce-binds)
                      body-af (->> tail rest (map walk) reduce-do)]
                  (fn [r e] (binds-af (fn [syms] (binding [recur-target (gensym "rec")]
                                                   (clojure.walk/prewalk identity ; `binding` doesn't play nice with lazy seq
                                                     `(letfn [(~recur-target [~@bind-names] ~(body-af r e))]
                                                        (~recur-target ~@syms))))) e)))

          recur (let [rec-vals-af (->> tail (map walk) reduce-call)]
                  (fn [r e] (rec-vals-af (fn [rec-vals-form] `(~recur-target ~@rec-vals-form)) e)))

          try (let [[body catches] (split-with #(not (and (seq? %) (= 'catch (first %)))) tail)
                    body-af (->> body (map walk) reduce-do)
                    catches-afs (->> catches (map (fn [[_ cls bnd & body]] [cls bnd (->> body (map walk) reduce-do)])))]
                (fn [r e]
                    (let [catches (->> catches-afs (map (fn [[cls bnd catch-af]] `(catch ~cls ~bnd ~(catch-af r e)))))]
                    `(try ~(body-af r (fn [body-error] (let [t (gensym "t")]
                                                               `(try (throw ~body-error) ~@catches (catch Throwable ~t ~(e t)))))) ~@catches))))

          throw (let [ex-af (walk (first tail))]
                  (fn [r e] (ex-af (fn [ex-form] (e ex-form)) e)))

          (throw (ex-info "Unknown special form" {:unknown-special-form head}))))
  
      (and (seq? form) (symbol? (first form)) (= #'await (resolve (first form))))
      (let [call-af (->> form rest (map walk) reduce-call)]
        (fn [r e] (call-af (fn [call-form]
                             (let [x (gensym "x")
                                   t (gensym "t")]
                               `(~@call-form
                                 (fn [~x] (try ~(r x) (catch Throwable ~t ~(e t))))
                                 (fn [~t] ~(e t))))) e)))

      (seq? form) (call)
      (vector? form) (map-af vec (call))
      (map? form) (->> form (mapcat identity) (map walk) reduce-call (map-af #(->> % (partition 2) (map vec) (into {}))))
      (set? form) (map-af set (call))
      
      :else pass)))

(defn async* [form]
  (let [af (->> form macroexpand-all walk)
        r (gensym "r")
        e (gensym "e")]
    `(fn [~r ~e] ~(af (fn [form] `(~r ~form)) (fn [form] `(~e ~form))))))

(defmacro async
  ([form]
   (async* form))
  ([form & forms]
  `(async (do ~form ~@forms))))

(defn deref [asc]
  (let [p (promise)]
    (asc #(deliver p [%]) #(deliver p [nil %]))
    (let [[x t] @p]
      (if t (throw t) x))))

(comment
  
  (defn swapi-demo [id]
    (require 'clj-http.client)
    (async
      (let [person-url (str "https://swapi.co/api/people/" id)
            person (:body (await clj-http.client/get person-url {:async? true :as :json}))
            homeworld-url (:homeworld person)
            homeworld (:body (await clj-http.client/get homeworld-url {:async? true :as :json}))]
        (str "Hi! I'm " (:name person) " from " (:name homeworld)))))

  "To run go"
  (deref (swapi-demo 1)))
