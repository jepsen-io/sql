# Jepsen SQL Workloads

These are common workloads for testing SQL databases using Jepsen and JDBC. A
Jepsen test for (e.g.) Postgres can use this library to generate transactions,
submit them over a JDBC client, and evaluate their correctness.

## Usage

SQL databases are all alike, except for the ways in which they are very
different. Your test needs to tell this one how to open a JDBC client and how
to interpret errors. The top-level entry point is `jepsen.sql/workloads`

```clj
(jepsen.sql/workloads
  {:open (fn [test node] ...)
   :error-fn (fn [e] ...)})
```

This returns a map of workload names (e.g. `:append`) to functions which take
CLI option maps and return a workload (e.g. `{:client ..., :generator ...,
:checker ...}`). You can use these workloads to build a test map.

Workloads use a set of standard keys in the test map, like `:wfr-keys?` and
`:max-writes-per-key`, which control various behaviors. These options are
documented in a data structure, `jepsen.sql/cli-opts`, which can be merged into
your CLI options for `tools.cli`.

### Opening a Client

You'll need a function `(open test node)` which takes a Jepsen test map and a
node string, and returns a `next.jdbc` Connection. For example:

```clj
(defn open
  "Opens a JDBC connection to the given node."
  [test node]
  (let [spec {:dbtype "meowdb"
              :host node
              :port port}]
    (->> spec
         j/get-datasource
         j/get-connection)))

```

### Errors

Each database and each client throw different types of exceptions, and with
different error messages. However, workloads need to understand at least a
little bit of what happened when an error is thrown. To do that, we transform
exceptions using an error function `(error-fn exception)`. For example:

```clj
(defn error-fn [e]
  (let [m (.getMessage e)]
    (condp identical? (class e)
      SQLException
      (condp re-find m
        #"duplicate key value" {:type :duplicate
                                :msg  m
                                :definite? true}
        ...)
      ...)))
```

`error-fn` should return either a Clojure map (which will be thrown as an
ExceptionInfo via Slingshot `throw+`), or `nil`, in which case the original
error is thrown as if nothing had happened.

Error maps must have a `:type` key, which may be any object. They can also
include a `:definite?` key, which should be `true` iff the operation performed
in the body which threw had no effects. Definite errors are converted to `:type
:fail` ops; all others are `:type :info`.

**This implies that any `try` form using this error handling should ensure its
effects are atomic.** It would be bad, for example, to run (outside a
transaction):

```clj
(c/with-errors op
  (j/execute! ["INSERT ..."])
  (j/execute! ["INSERT ..."]))
```

If the first `INSERT` succeeded, but the second threw an error which had
`:definite? true`, this `with-errors` form would return a `:type :fail`, even
though some of its effects took place. The clients in this library are careful
not to do this; if you write your own, you need to take care too.

Error maps can have other keys at your discretion.

## License

Copyright © 2026 Jepsen, LLC

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
https://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
