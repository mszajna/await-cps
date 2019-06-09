# await-cps

async/await macros for continuation-passing style clojure libraries (async ring, clj-http...)

## Usage

```clojure
(require '[await-cps :refer [async await]]
         '[clj-http.client :refer http])

(defn swapi-handler [request respond raise]
  (async respond raise
    (let [person-url (str "https://swapi.co/api/people/" (:id (:params request)))
          person (:body (await http/get person-url {:async? true :as :json}))
          homeworld-url (:homeworld person)
          homeworld (:body (await http/get homeworld-url {:async? true :as :json}))]
      (str "Hi! I'm " (:name person) " from " (:name homeworld)))))

(swapi-handler {:params {:id 1}} println println)
```
