(ns await-cps
  (:refer-clojure :exclude [await deref])
  (:require [clojure.walk :refer [macroexpand-all]]))

; 'af' or 'async-form' is a function (fn [r e] form) and returns a form
; 'r' and 'e' are functions that given a form return a new form

(defn map-af [f af]
  (fn [r e] (af (comp r f) e)))

(defn mapcat-af [f af]
  (fn [r e] (af (fn [form] ((f form) r e)) e)))

(defn catch-af [f af]
  (fn [r e] (af r (comp r f))))

(defn catchcat-af [f af]
  (fn [r e] (af r (fn [form] ((f form) r e)))))

(defn inline-form [form]
  (fn [r e] (r form)))

(defn throw-form [form]
  (fn [r e] (e form)))

(defn sync-form [form]
  (let [x (gensym "x")]
    (fn [r e] `(let [~x ~form] ~(r x)))))

(defn async-form [form]
  (let [x (gensym "x")
        t (gensym "t")]
    (fn [r e] `(~@form
                 (fn [~x] (try ~(r x) (catch Throwable ~t ~(e t))))
                 (fn [~t] ~(e t))))))

(defn reduce-do [afs]
  (reduce (fn ([xs x] (fn [r e] (xs (fn [xs-form] `(do ~xs-form ~(x r e))) e)))
              ([] (inline-form nil)))
          afs))

(defn inline-allowed? [form]
  (not (coll? form)))

(defn reduce-call [afs]
  (->> afs
       (map #(mapcat-af (fn [form] (if (inline-allowed? form) (inline-form form) (sync-form form))) %))
       (reduce (fn [xs x] (fn [r e] (xs (fn [xs-form] (x (fn [x-form] (r `(~@xs-form ~x-form))) e)) e)))
               (inline-form ()))))

(defn reduce-if [[test-af & afs]]
  (fn [r e] (test-af (fn [test-form] `(if ~test-form ~@(map #(% r e) afs))) e)))

(defn reduce-case [exp-af afs]
  (fn [r e] (exp-af (fn [exp-form] `(case ~exp-form ~@(mapcat (fn [[case af]] (if af [case (af r e)] [(case r e)])) afs))) e)))

(defn reduce-binds [binds]
  (reduce (fn [xs [sym x]] (fn [r e] (xs (fn [_] (x (fn [x-form] `(let [~sym ~x-form] ~(r sym))) e)) e)))
          (inline-form nil)
          binds))

(def ^:dynamic recur-target)

(defn dbg [x] (println x) x)

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

          let* (let [binds (->> tail first (partition 2) (map (fn [[sym form]] [sym (walk form)])) reduce-binds)
                     body (->> tail rest (map walk) reduce-do)]
                (->> binds (mapcat-af (fn [_] body))))
          if (->> tail (map walk) reduce-if)
          do (->> tail (map walk) reduce-do)
          throw (->> tail first walk (mapcat-af throw-form)) ; TODO: error on multiple tail

          case* (let [[ge shift mask default imap & args] tail
                      default-af (walk default)
                      imap-af (fn [r e] (->> imap
                                             (map (fn [[hash [val form]]] [hash [val ((walk form) r e)]]))
                                             (into {})))]
                  (fn [r e] `(case* ~ge ~shift ~mask ~(default-af r e) ~(imap-af r e) ~@args)))
          
          try (let [[body catches] (split-with #(not (and (seq? %) (= 'catch (first %)))) tail)
                    body-af (->> body (map walk) reduce-do)
                    catches-afs (->> catches (map (fn [[_ cls bnd & body]] [cls bnd (->> body (map walk) reduce-do)])))]
                (fn [r e]
                    (let [catches (->> catches-afs (map (fn [[cls bnd catch-af]] `(catch ~cls ~bnd ~(catch-af r e)))))]
                    `(try ~(body-af r (fn [body-error] (let [t (gensym "t")]
                                                               `(try (throw ~body-error) ~@catches (catch Throwable ~t ~(e t)))))) ~@catches))))
          loop* (let [bind-names (->> tail first (partition 2) (map first))
                      bind-vals-af (->> tail first (partition 2) (map #(walk (second %))) reduce-call)
                      body-af (->> tail rest (map walk) reduce-call)]
                  (fn [r e] (bind-vals-af (fn [bind-vals-form] (binding [recur-target (gensym "rec")]
                                                                 (clojure.walk/prewalk identity ; `binding` doesn't play nice with lazy seq
                                                                   `(letfn [(~recur-target [~@bind-names] ~(body-af r e))]
                                                                      (~recur-target ~@bind-vals-form))))) e)))
          recur (let [rec-vals-af (->> tail (map walk) reduce-call)]
                  (fn [r e] (rec-vals-af (fn [rec-vals-form] `(~recur-target ~@rec-vals-form)) e)))

          (throw (ex-info "Unknown special form" {:unknown-special-form head}))))
  
      (and (seq? form) (= `await (first form))) (->> form rest (map walk) reduce-call (mapcat-af async-form))
  
      (seq? form) (call)
      (vector? form) (map-af vec (call))
      (map? form) (->> form (mapcat identity) (map walk) reduce-call (map-af #(->> % (partition 2) (map vec) (into {}))))
      (set? form) (map-af set (call))
      
      :else pass)))

(defn await [f & args]
  (throw (ex-info "Await called outside async block" {})))

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
