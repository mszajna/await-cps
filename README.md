# await-cps

async/await for continuation-passing style functions

[![Latest version on Clojars](https://clojars.org/await-cps/latest-version.svg)](https://clojars.org/await-cps)

Continuation-passing style ([CPS](https://en.wikipedia.org/wiki/Continuation-passing_style))
is a pattern where a function, instead of returning a value, calls the provided
continuation function with that value. It's used in popular Clojure libraries
([Ring's async handlers](https://github.com/ring-clojure/ring/wiki/Concepts#handlers),
[clj-http](https://github.com/dakrone/clj-http#async-http-request))
to avoid blocking the calling thread by long-running IO operations.

Writing correct CPS code is awkward and
[hard to get right](#how-is-writing-correct-cps-code-by-hand-hard) however.
Any non-trivial flow can quickly become unmanageable. This library delivers
async/await expressions that let you write idiomatic, synchronous-looking code
while leveraging the power of asynchronous, continuation-passing style functions.

[API Docs](https://cljdoc.org/d/await-cps/await-cps/CURRENT/api/await-cps)

## Usage

`await` the execution of asynchronous function in an `async` block just like
it was a blocking function call.

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

Use `defn-async` for brevity.

```clojure
(defn-async star-wars-greeting-handler [request]
  (let [person-url (str "https://swapi.co/api/people/" (:id (:params request)))
        person (:body (await http/get person-url {:async? true :as :json}))]
    ...))
```

### Asynchronous functions

Asynchronous function is any function that takes `resolve` and `raise` callbacks
as the last two parameters. It is expected to evetually either call `resolve`
with the result value, call `raise` with a `Throwable` or throw in the calling
thread. The return value is ignored.

You can await the completion of asynchronous function in an `async` block
calling `await` with the function and any parameters, leaving the callbacks
out. You're free to use await wherever a function call is allowed.
The execution does not block the calling thread, resuming in whatever
thread the awaited function invokes the callback in instead. Awaiting will
observe the resolved value as if it was returned or any exception thrown or
raised as if it was thrown.

### Asynchronous block boundary

`async` body can handle arbitrary Clojure code. Its boundary does not stretch
inside nested `def`s nor the body of any nested function however.
This includes `def`, `fn`, `reify`, `deftype` and functions in `letfn`.
The code below does *not* work:

```clojure
(async resolve raise
  (doall (map (fn [url] (await http/get url {:async true}))
              ["https://google.com" "https://twitter.com"])))
=> IllegalStateException await called outside async block
```

Use `loop/recur` to traverse collections.

```clojure
(async resolve raise
  (loop [[url & urls] ["https://google.com" "https://twitter.com"]]
    (when url
      (println (:body (await http/get url {:async? true})))
      (recur urls))))
```

You can also use `fn-async` for ad-hoc asynchronous functions.

```clojure
(async resolve raise
  (loop [[f & fs] (map (fn [url] (fn-async []
                                   (:body (await http/get url {:async? true}))))
                       ["https://google.com" "https://twitter.com"])]
    (when f
      (println (await f))
      (recur fs))))
```

### Recurring

Recurring is supported in the context of `fn-async`, `defn-async` and `loop`
within `async` block.

### try/catch/finally

`try/catch/finally` is fully supported. Note however that if a CPS function fails
to call either the resolve or raise callback the `finally` block may never execute.
This would be equivalent to
[killing a thread](https://docs.oracle.com/javase/tutorial/essential/exceptions/finally.html)
that's executing a regular `try` block.

### Monitor operations

`monitor-enter` and `monitor-exit` (and by extension the `locking` macro)
are JVM's low level concurrency primitives strictly bound to executing thread
and therefore are not supported in `async` blocks.
Used across asynchronous call will lead to concurrency bugs.
Currently there's no warning if this is to happen.

## Does it work?

Being cautious about third-party software applying chainsaw surgery to your
production code is only fair. The goal of this library is for you to be able
to use it with confidence.

The test suite included employs generative testing producing nested combinations
of expressions, including special forms, synchronous and asynchronous function
calls, failures and side-effects.
It asserts that both the result (value returned or exception thrown) and
the order of any side-effects is consistent with what you'd observe executing
synchronously in a single thread.

At the same time, the project has not seen extensive production use yet,
use with caution. Please, raise any issues through
[GitHub](https://github.com/mszajna/await-cps/issues).

## How is writing correct CPS code by hand hard?

Even though a bit awkward, a CPS implementation of the happy path tends to be
straightforward enough. Covering exceptional cases, however, is a whole lot
harder.

### Uncaught exceptions

Any exceptions not handled by `resolve` callback will likely
[get swallowed](https://stuartsierra.com/2015/05/27/clojure-uncaught-exceptions).
Just to be safe you should wrap all your `resovle` functions in a catch-all
calling `raise`

### Equivalent for try/catch/finally

Writing correct CPS equivalent for `try/catch/finally` block is about
the trickiest problem this library solves. It needs to:
- handle all 3 exit modes of asynchronous function (exception thrown,
  resolve called, raise called)
- correctly scope the try/catch block outside the asynchronous function as well
  as inside the `resolve` callback
- handle asynchronous body as well as asynchronous catches and finally
- make sure finally is only ever run once in all cases above.

The amount of edge cases is exactly what inspired the use of generative testing
in this library. I strongly recommend avoiding roll-your-own solutions without
thorough coverage.

Even if you are confident that you can get an equivalent for `try/catch/finally`
right, any `try/finally` facilities (like `with-open`, `binding` and others)
won't ever work across asynchronous calls without transforming the macroexpanded
form.

## License

This project is distributed under [The MIT License](https://github.com/mszajna/await-cps/blob/master/LICENSE).
