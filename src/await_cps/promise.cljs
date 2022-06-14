(ns await-cps.promise
  "Interoperability with JavaScript Promise.")

(defn ->promise [cps-fn & args]
  "Applies cps-fn to args plus two continuations that resolve or reject the
   Promise returned."
  [cps-fn & args]
  (new js/Promise (apply partial cps-fn args)))

(defn fulfilled [promise resolve raise]
  "When promise gets resolved successfully, calls resolve with the value.
   If it is rejected, calls raise with the exception.
   Use with await: (await fulfilled cf)"
  (.then promise resolve raise))
