# await-cps

async/await for continuation-passing style (CPS) functions

This library delivers async/await expressions for use with asynchronous
functions that take resolve and raise callbacks as the last parameters
(e.g. ring, clj-http...).

## Usage

```clojure
(require '[await-cps :as cps]
         '[clj-http.client :as http])

(defn swapi-handler [request respond raise]
  (cps/async respond raise
    (let [person-url (str "https://swapi.co/api/people/" (:id (:params request)))
          person (:body (cps/await http/get person-url {:async? true :as :json}))
          homeworld-url (:homeworld person)
          homeworld (:body (cps/await http/get homeworld-url {:async? true :as :json}))]
      (str "Hi! I'm " (:name person) " from " (:name homeworld)))))

(swapi-handler {:params {:id 1}} println println)
```

or with custom `defn`

```clojure
(require '[await-cps :refer cps]
         '[clj-http.client :as http])

(cps/defn-async swapi-handler [request]
  (let [person-url (str "https://swapi.co/api/people/" (:id (:params request)))
        person (:body (cps/await http/get person-url {:async? true :as :json}))
        homeworld-url (:homeworld person)
        homeworld (:body (cps/await http/get homeworld-url {:async? true :as :json}))]
    (str "Hi! I'm " (:name person) " from " (:name homeworld))))

(swapi-handler {:params {:id 1}} println println)
```

## TODO

- presereve meta
- test loop/recur for stack overflows
- recursive fn-async
- sanitise monitor-*
