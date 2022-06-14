# Parallel execution design sheet

Modes:
- first result or first failure (FRFF)
  - C# `Task.WaitAny`
  - JS `Promise.race`
  - turns deterministic bugs into non-deterministic bugs
- first result or all failures (FRAF)
  - JS `Promise.any`
    - symmetric to `Promise.all`
    - experimental and controversial feature
  - obscures bugs turning them into dead code
  - can be used in place of FRFF but not the other way around
  - can be implemented using ARFF flipping resolve/raise, but that's not obvious to end users
  - seems I'll have to implement this one, for completeness at least
  - how to avoid obscuring bugs?
    - a dynamic var handler? feels like an afterthought
- all results or first failure (ARFF)
  - C# `Task.WaitAll`
  - JS `Promise.all`
  - can be used in place of ARAF but not the other way around
  - possible names: plet, pllet, letp, par
    - `let`-like names suggest you can use bound names in other bindings
    - manifold delivers something like that, where execution graph is inferred
  - only the first exception is handled but that's enough to surface problems
  - built in pmap has similar characteristics
- all results or all failures (ARAF)
  - JS `Promise.allSettled` (somewhat, allSettled never rejects but I could raise with all failures)
  - no exceptions are being suppressed, not just if failed but everything that failed exactly
  - possible names: all (confusing with Promise.all, otherwise more appropriate here)
- tons of other custom ones
  - perhaps they could be expressed as a combination of above?
  - blubird.js has .some combinator for first N successful promises

## Do I want an inferred parallelism?

TL;DR: it's cool but challenging
---

Manifold does it with `let-flow`. I like the name `plet`. It would look like this:

```clojure
(plet [a (await x)
       b (await y a) ; sequential with a
       c (await z)]) ; parallel with a & b
```

How useful is it? What it would look like implemented with `par`?

```clojure
(par [[a b] (let [a (await x)
                  b (await y a)] [a b])
      c (await z)])
```

`plet` is generalised `par`. With `par` parallelism is guaranteed, with `plet`
it's inferred. With `plet` the execution flow is not clear without looking into
expressions on the right. It's a partial ordering from the top to bottom.
I'd need something like `par` to implement `plet` anyway I guess.
Some `plet`s reduce to just `let` which should be discouraged.

Parsing `plet` is tricky because stuff can be rebound. Maybe I'll do it later?
But once I've got `plet`, `par` is deprecated I guess?

```clojure
(plet [a (await x)
       b (await y a)
       c (await z (let [a 1] a)) ; a is rebound internally
       a (await x) ; a is rebound here on
       d (await y a)])
```