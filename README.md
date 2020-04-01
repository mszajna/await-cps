# await-cps

**async/await** syntax for functions that take a successful- and an exceptional
callback in the last two arguments, a pattern known as
[continuation-passing style](https://en.wikipedia.org/wiki/Continuation-passing_style)
and popularised by
[Ring](https://github.com/ring-clojure/ring/wiki/Concepts#handlers)
and
[clj-http](https://github.com/dakrone/clj-http#async-http-request).

[![Latest version on Clojars](https://clojars.org/await-cps/latest-version.svg)](https://clojars.org/await-cps)

[See API docs](https://cljdoc.org/d/await-cps/await-cps/CURRENT/api/await-cps)

## Motivation

Continuation-passing style (CPS) is commonly used in Clojure to implement
asynchronous flow. For example, a Ring handler could look like this:

```clojure
(require '[clj-http.client :as http] ; the example requires clj-http
         'cheshire.core)             ; and cheshire

; asynchronous Ring handlers are CPS functions
(defn sw-handler [request respond raise]
  (let [person-id (get-in request [:params :id])
        person-url (str "https://swapi.co/api/people/" person-id)]
    ; http/get with :async? option on is a CPS function too
    (http/get person-url
              {:async? true :as :json}
              (fn success [response]
                (let [name (get-in response [:body :name])]
                  (respond {:status 200 :body (str "Hi! I'm " name)})))
              (fn error [exception]
                (respond {:status 500 :body "server error"})))))
```

Readability aside, callback-based code tends to suffer from a number of issues:
- the exceptional case seems to be handled, but, as it turns out, CPS functions
  often throw in the calling thread instead of raising through the callback,
- any unhandled exception in either of the continuations has an undefined
  behaviour, at best, being
  [silently swallowed](https://stuartsierra.com/2015/05/27/clojure-uncaught-exceptions),
  at worst, compromising liveliness of the application,
- `try/finally`-based facilities do not work across callback invocations, and
  implementing `finally` by hand is notoriously hard.

Neither of the problems is unfixable but writing correct callback-based code is
difficult, laborious and requires rigorous discipline. *await-cps* delivers
**async/await** syntax for effortless CPS code that reads like idiomatic
Clojure.

## Usage

Asynchronous function, defined with `defn-async` or `afn`, is a CPS function
where continuations are declared and invoked implicitly. Within its body, any
CPS function can be `await`-ed creating the illusion of a blocking call.

```clojure
(require '[await-cps :refer [defn-async afn await await!]])

; expands to (defn sw-handler [request respond raise] ...)
(defn-async sw-handler [request]
  (let [person-id (get-in request [:params :id])
        person-url (str "https://swapi.co/api/people/" person-id)]
    (try            ; await does not actually block the thread
      (let [response (await http/get person-url {:async? true :as :json})
            name (get-in response [:body :name])]
        ; respond callback is called with the result implicitly
        {:status 200 :body (str "Hi! I'm " name)})
      (catch Exception e
        ; handles both, exceptions thrown by http/get and its async errors
        {:status 500 :body "server error"}))))
    ; any unhandled exceptions will raise
```

Asynchronous functions are regular CPS functions taking a successful- and an
exceptional callback, one of which is eventually invoked (unless the function
throws).

```clojure
(sw-handler
  {:params {:id 1}}
  println                              ; first callback takes the result value
  #(println (.getMessage %)))          ; second callback takes the exception
;=> nil                                ; the return value isn't very useful
;=> {:status 200
;=>  :body "Hi! I'm Luke Skywalker"}   ; but the result is eventually printed
```

Use `afn` to define an ad-hoc asynchronous function. `await!` is the blocking
`await`.

```clojure
(await!
  (afn [] (:status (await http/get "https://twitter.com" {:async? true}))))
;=> 200
```

### Asynchronous scope

Use of `await` is limited to the body of the asynchronous function defined with
`defn-async` or `afn`. It does not extend to any functions defined within,
using `fn`, `letfn`, `reify` or `deftype`. Note that some macros, like `for`,
expand to a function and will not support awaiting either.

```clojure
(await!
  (afn []
     (for [url ["https://google.com" "https://twitter.com"]]
       (:status (await http/get url {:async? true})))))
;=> IllegalStateException await called outside asynchronous scope
```

For collection traversal use `loop`/`recur`, or `doseq`, if you are traversing
for the side effects only.

```clojure
(await!
  (afn []
    (loop [[url & urls] ["https://google.com" "https://twitter.com"]
           statuses []]
      (if url
        (let [status (:status (await http/get url {:async? true}))]
          (recur urls (conj statuses status)))
        statuses))))
;=> [200 200]
```

### Execution

This library does not come with an executor. Continuations execute in whatever
thread the callback is invoked in. This is a simple and efficient model that
works well with non-blocking code. Use `blocking` macro to execute blocking
operations in a separate thread and avoid performance and liveliness issues.

```clojure
(await!
  (afn []
    (await (blocking
             (slurp "some-large-file")))))
```

### Interoperability

`await-cps.java/future-call` applies a CPS function and returns its result as
a `CompletableFuture`.

Within an asynchronous function `CompletableFuture`s can be awaited using
`await-cps.java/complete`. *Manifold* and *core.async* deliver awaitable
CPS functions for `deferred`s and channels.

```clojure
(require '[await-cps.java :as j]
         '[manifold.deferred :as d]
         '[clojure.core.async :as a])
(import java.util.concurrent.CompletableFuture)

(await!
  (afn []
    (= (await j/complete (CompletableFuture/completedFuture :value))
       (await d/on-realized (d/success-deferred :value))
       (let [ch (a/chan 1)]
         (await a/put! ch :value) ; put! and take! are not strictly CPS but are
         (await a/take! ch)))))   ; guaranteed to work with await nonetheless
;=> true
```

When rolling your own integration, bear in mind the pitfalls of writing
callback-based code listed at the top.

## Maturity

The project has seen limited production use, but it tries to make it up with
thorough test coverage an generative testing. Arbitrary asynchronous functions
are generated, and the behaviour is asserted to match what you would expect to
observe executing it synchronously.

The core API including `defn-async`, `afn` and `await` is considered stable.

## License

This project is distributed under
[The MIT License](https://github.com/mszajna/await-cps/blob/master/LICENSE).
