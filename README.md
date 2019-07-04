# await-cps

async/await for continuation-passing style functions

[![Latest version on Clojars](https://clojars.org/await-cps/latest-version.svg)](https://clojars.org/await-cps)

[API Docs](https://cljdoc.org/d/await-cps/await-cps/CURRENT/api/await-cps)

## Usage

Writing correct CPS code is hard and awkward. Exceptions thrown in continuation
functions tend to get swallowed. Any non-trivial flow can quickly become
unmanageable.

This library delivers async/await expressions that let you write idiomatic,
synchronous-looking code while leveraging the power of asynchronous,
continuation-passing style functions
([Ring's async handlers](https://github.com/ring-clojure/ring/wiki/Concepts#handlers),
[clj-http](https://github.com/dakrone/clj-http#async-http-request)).

```clojure
(require '[await-cps :refer [async await]]
         '[clj-http.client :as http])

(defn star-wars-greeting-handler [request respond raise]
  ; initiate the async block
  (async respond raise
    (let [person-url (str "https://swapi.co/api/people/" (:id (:params request)))
          ; await the completion of asynchronous http request, doesn't block the thread 
          person (:body (await http/get person-url {:async? true :as :json}))]
      (str "Hi! I'm " (:name person) " from "
           ; await expression can go wherever a function call is allowed
           (get-in (await http/get (:homeworld person) {:async? true :as :json})
                   [:body :name])))))
```

You can also use `defn-async` to reduce boilerplate.

```clojure
(defn-async star-wars-greeting-handler [request]
   ...)
```

Async block can handle arbitrary Clojure code with following limitations:

### loop/recur

Recurring is supported in the context of a loop (not yet in the context of a function).

```clojure
(async respond raise
  (loop [offset 0]
    (println (:body (await http/get (str "https://google.com/search?q=clojure&start=" offset) {:async? true})))
    (recur (+ 10 offset))))
```

When awaiting in a loop if awaited function invokes the continuation in
the calling thread the call stack will keep growing until overflow. This could
be a problem for libraries that take the CPS as an argument and can't make
runtime assumptions about it. A workaround could be wrapping callbacks in a future.

```clojure
(await (fn [resolve raise] (arbitrary-cps-fn args* #(future (resolve %)) #(future (raise %)))))
```

### try/catch/finally

`try/catch/finally` is fully supported. Note however that when a CPS function
completes failing to call either the resolve or raise callback the `finally` block
may never execute. This would be equivalent to
[killing a thread](https://docs.oracle.com/javase/tutorial/essential/exceptions/finally.html)
that's executing a regular `try` block.

### Monitor operations

`monitor-enter` and `monitor-exit` are strictly related to executing thread and
therefore are not supported.

## TODO

- presereve meta
- recursive fn-async
- warn on monitor-*
