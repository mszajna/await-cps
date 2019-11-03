# await-cps

async/await syntax for continuation-passing style functions

[![Latest version on Clojars](https://clojars.org/await-cps/latest-version.svg)](https://clojars.org/await-cps)

Continuation-passing style
([CPS](https://en.wikipedia.org/wiki/Continuation-passing_style))
is a pattern where a function takes an extra 'continuation' argument
(a callback) and invokes it with
the result instead of returning the value. In Clojure ecosystem a popular
flavour of this is to take two continuations for successful and exceptional
outcome. It is used by Ring for
[asynchronous handlers](https://github.com/ring-clojure/ring/wiki/Concepts#handlers)
and by clj-http for
[asynchronous responses](https://github.com/dakrone/clj-http#async-http-request)
with the goal of avoiding blocking of the calling thread with long-running
IO operations.

Writing correct CPS code is awkward and
[hard to get right](#how-is-writing-correct-cps-code-by-hand-hard) however.
Any non-trivial flow can quickly become unmanageable. This library delivers
async/await syntax that lets you write idiomatic, synchronous-looking code
while leveraging the power of asynchronous, continuation-passing style functions.

[API Docs](https://cljdoc.org/d/await-cps/await-cps/CURRENT/api/await-cps)

## Usage

```clojure
(require '[await-cps :refer [defn-async afn await await!]]
         '[clj-http.client :as http]
         'cheshire.core) ; required for :as :json

; a naive, hand-crafted CPS function could look like this
(defn star-wars-greeting [person-id resolve raise]
  (let [person-url (str "https://swapi.co/api/people/" person-id)]
    ; clj-http.client/get with {:async? true} is an example of a CPS function
    (http/get person-url {:async? true :as :json}
              (fn [result]
                (let [person (:body result)]
                  ; it gets awkward pretty quick with nested calls
                  ; also, unhandled http/get exceptions are swallowed
                  (http/get (:homeworld person) {:async? true :as :json}
                            (fn [result]
                              (resolve (str "Hi! I'm "
                                            (:name person)
                                            " from "
                                            (get-in result [:body :name]))))
                            raise)))
              raise)))

; let's redefine it as an asynchronous function
(defn-async star-wars-greeting
  [person-id] ; continuation arguments are injected for you and invoked implicitly
  (let [person-url (str "https://swapi.co/api/people/" person-id)
                      ; await does not block the calling thread
        person (:body (await http/get person-url {:async? true :as :json}))]
    (str "Hi! I'm " (:name person) " from "
                 ; you can put your awaits wherever you please
         (get-in (await http/get (:homeworld person) {:async? true :as :json})
                 [:body :name]))))

; use await! to run it synchronously in the REPL
(await! star-wars-greeting 1)
;=> "Hi! I'm Luke Skywalker from Tatooine"

; asynchronous functions are regular CPS functions
(clojure.repl/doc star-wars-greeting)
;=> user/star-wars-greeting
;=> ([person-id &resolve &raise])

; you can invoke them directly providing callbacks
(star-wars-greeting 5
  ; success callback takes the function result
  println
  ; failure callback takes the exception
  #(println "Oops" (.getMessage %)))
;=> nil
; notice that the result arrives asynchronously
;=> "Hi! I'm Leia Organa from Alderaan"

; afn defines an inline asynchronous function
(await! (afn []
          (str "RRWWWGG => "
               ; asynchronous functions are CPS and so awaitable
               (await star-wars-greeting 13))))
;=> "RRWWWGG => Hi! I'm Chewbacca from Kashyyyk"

; asynchronous functions of one argument are valid Ring async handlers
(defn-async star-wars-greeting-handler
  [request] ; expands to [request respond raise]
  (if-let [person-id (get-in request [:params :id])]
    {:status 200 :body (await star-wars-greeting person-id)}
    {:status 400 :body "id parameter required"}))
```

### await

`await` works within the scope of an asynchronous function defined with
`defn-async` and `afn`. Although it's not technically a function you can place
it wherever a function call is syntactically allowed. It executes the CPS
function along with any arguments provided plus two generated continuations.
The continuations are generated based on the surrounding code to create the
illusion of `await` blocking the flow.

It does not block the calling thread however. The control is passed to the CPS
function and the execution continues in the thread continuation is invoked in.

Awaitable CPS functions are expected to take two continuation functions as
their last parameters and eventually invoke one of them. It is acceptable for
a CPS function to throw in the calling thread. The return value of both,
the CPS function and the continuation is ignored. The second, exceptional
continuation only accepts a `Throwable`.

Asynchronous functions produced by `defn-async` and `afn` are always awaitable.

### Asynchronous function's boundary

Functions defined inside an asynchronous function cannot be implicitly made
asynchronous themselves. `await` won't work in any nested `fn`, `reify`, `def`,
`deftype` or a function bound in `letfn`.

```clojure
(await! (afn []
          (doall (map (fn [url] (:status (await http/get url {:async true})))
                      ["https://google.com" "https://twitter.com"]))))
;=> IllegalStateException await called outside async block

; use loop/recur to traverse collections
; or doseq if you're traversing for side effects only

(await! (afn []
          (loop [[url & urls] ["https://google.com" "https://twitter.com"]
                 statuses []]
            (if url
              (recur urls (conj statuses (:status (await http/get url {:async? true}))))
              statuses))))
;=> [200 200]
```

### recur

Recurring a `defn-async` or `afn` function, the implicit continuation arguments
are omitted.

```clojure
(await! (afn [[url & urls]]
          (when url
            (println url (:status (await http/get url {:async? true})))
            (recur urls)))
        ["https://google.com" "https://twitter.com"])
```

### try/catch/finally

`try/catch/finally` is fully supported. Note however that if a CPS function fails
to call either the resolve or raise callback the `finally` block may never execute.
This would be equivalent to
[killing a thread](https://docs.oracle.com/javase/tutorial/essential/exceptions/finally.html)
that's executing a regular `try` block.

### Monitor operations

`monitor-enter` and `monitor-exit` are JVM's low level concurrency primitives
strictly bound to the executing thread and are not supported in asynchronous
functions (and as a consequence neither is the `locking` macro).

Used across asynchronous calls will lead to concurrency bugs.
Currently there's no warning if this is to happen.

## Does it work?

Being cautious about third-party software applying chainsaw surgery to your
production code is only fair. A goal of this library is for you to be able
to use it with confidence.

The test suite included employs generative testing producing nested combinations
of expressions, including special forms, synchronous and asynchronous function
calls, exceptions and side-effects.
It asserts that both the result (value returned or exception thrown) and
the order of any side-effects is consistent with what you'd observe executing
synchronously in a single thread.

At the same time, the project has not seen extensive production use yet.
Use with caution. Please, raise any issues through
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
the trickiest problem this library tackles. It needs to:
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
