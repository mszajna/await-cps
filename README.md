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

Async block can handle arbitrary Clojure code with following notes:

### async boundary

The boundary of async block does not stretch inside the body of any nested
function defined within.

```clojure
(async resolve raise
  (doall (map (fn [url] (await http/get url {:async true}))
              ["https://google.com" "https://twitter.com"])))
=> IllegalStateException await called outside async block
```

Use `loop/recur` to traverse collections.

```clojure
(async resolve raise
  (loop [[x & xs] ["https://google.com" "https://twitter.com"]]
    (when x
      (println (await http/get x {:async? true}))
      (recur xs))))
```

### Recurring

Recurring is supported in the context of a `loop`, `fn-async` and `defn-async`.

When awaited function invokes continuation in the calling thread the call
stack may keep growing until overflow. This is most problematic for code that
can't make assumptions about runtime properties of the function awaited.
You can wrap it in `with-new-call-stack` to avoid the issue at the penalty
of extra time taken to schedule a task each loop.

### try/catch/finally

`try/catch/finally` is fully supported. Note however that when a CPS function
completes failing to call either the resolve or raise callback the `finally` block
may never execute. This would be equivalent to
[killing a thread](https://docs.oracle.com/javase/tutorial/essential/exceptions/finally.html)
that's executing a regular `try` block.

### Monitor operations

`monitor-enter` and `monitor-exit` are strictly related to the executing thread
and therefore are not supported.

Currently there is no warning when monitor-* is used inside async which
may lead to hard-to-spot concurrency bugs.

### Meta

Metadata of forms containing `await` expressions is currently lost in translation.

## Does it work?

Being cautious about a library applying chainsaw surgery to your production
code is only fair. The goal of this library is for you to be able to use it
with confidence.

The test suite included employs generative testing producing nested combinations
of expressions including special forms, synchronous and asynchronous function
calls. It asserts that both the result (value returned or exception thrown) and
the order of any side effects is consistent with what you'd observe with
regular, synchronous evaluation.

At the same time, the project has not seen extensive production use yet.

## License

This project is distributed under [The MIT License](https://github.com/mszajna/await-cps/blob/master/LICENSE).
