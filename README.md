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
(require '[await-cps :as cps]
         '[clj-http.client :as http])

(cps/defn swapi-handler [request]
  (let [person-url (str "https://swapi.co/api/people/" (:id (:params request)))
        person (:body (cps/await http/get person-url {:async? true :as :json}))
        homeworld-url (:homeworld person)
        homeworld (:body (cps/await http/get homeworld-url {:async? true :as :json}))]
    (str "Hi! I'm " (:name person) " from " (:name homeworld))))

(swapi-handler {:params {:id 1}} println println)
```

```clojure
(cps/defn swapi-handler [request]
  (let [person-url (str "https://swapi.co/api/people/" (:id (:params request)))
        person (:body (cps/await :timeout 1000
                                 http/get person-url {:async? true :as :json}))
        [mirror result] (cps/alts (await cps/ms 100 [:timeout])
                                  [:mirror1 (await http/get "http://mirror1/")]
                                  [:mirror2 (await http/get "http://mirror2")])
        [a b] (cps/all (await a) (+ 1 (await b)))]))

```

## TODO

- presereve meta
- test loop/recur for stack overflows
- primitives for concurrency and timeouts
- reconsider the API (would be nice for cps/defn to include the word async)
- ensure only one of the handlers will be called and only once even when async functions are buggy
- test parallel stuff
