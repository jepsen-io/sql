(ns jepsen.sql.gen
  "Test.check generators for simple schemas and statements."
  (:require [clojure [core :as c]
                     [math :refer [round]]
                     [pprint :refer [pprint]]
                     [set :as set]
                     [string :as str]]
            [clojure.test.check :as tc]
            [clojure.test.check [generators :as g]]
            [clojure.tools.logging :refer [info warn]]
            [jepsen [random :as rand]]
            [dom-top.core :refer [loopr]]))

;; Basic generators

(defn bind
  "It is SO frustrating that tc/gen does (fmap f gen) but (bind gen f)."
  [f gen]
  (g/bind gen f))

(defmacro flet
  "A reduced version of `generators/let` which uses `fmap`, rather than `bind`,
  and does not support serial assignment; all bindings must be independent of
  one another. My hope is this shrinks better."
  [bindings & body]
  (assert (even? (count bindings)))
  (let [pairs  (partition 2 bindings)
        lefts  (mapv first pairs)
        rights (mapv second pairs)]
    `(g/fmap (fn ~'flet [~lefts]
               ~@body)
             (g/tuple ~@rights))))

(defn probability
  "A generator which returns true with probability p, otherwise false. Shrinks
  towards false. Only works up to one part in 1000."
  [p]
  (let [scale 1000
        t     (max 1 (Math/round (float (* scale p))))]
    (g/fmap (fn [x]
              (< x t))
            (g/choose 0 (dec scale)))))

(defn rarely
  "A generator which rarely generates from `rare`, but sometimes from `common`."
  [rare common]
  (g/frequency [[100 common]
                [1 rare]]))

(defn gen-long
  "A generator of Long values between lower and upper, inclusive."
  [lower upper]
  (g/large-integer* {:min lower :max upper}))

;; Basic types, table and column names

(def column-names
  "All column names, as a vector."
  (mapv str "abcdefghijklmnopqrs"))

(def table-names
  "All table names, as a vector."
  ["t" "u" "v"])

(def all-types-vec
  "All possible types, in shrinking order"
  [:int])

(defprotocol SQL
  (sql [this] "Converts this AST node to an SQL vector suitable for use with
              `j/execute!"))

(defprotocol DDL
  (setup [this]
    "A vector of SQL vectors that will create this thing.")
  (teardown [this]
    "A vector of SQL vectors that will tear dwon this thing."))

(defn splice
  "Splices n SQL vectors or strings together. We do this by concatenating
  their strings and remaining parameters. Nil vanishes."
  ([a]
   a)
  ([a b]
   (cond (nil? a) b
         (nil? b) a
         true (if (string? a)
                (if (string? b)
                  (str a b)
                  (assoc b 0 (str a (first b))))
                (if (string? b)
                  (assoc a 0 (str (first a) b))
                  (-> (assoc a 0 (str (first a) (first b)))
                      (into (subvec b 1)))))))
  ([a b & more]
   (reduce splice
           (splice a b)
           more)))

(defn splice*
  "Splices n SQL strings or vectors together, optionally joining them with the
  given separator."
  ([sqls]
   (reduce splice sqls))
  ([separator sqls]
   (reduce (fn [acc sql]
             (-> acc
                 (splice separator)
                 (splice sql)))
           sqls)))

(defrecord Column
  [name
   type
   primary-key?]
  SQL
  (sql [_]
    [(str name " "
          (case type
            (c/name type))
          (when primary-key? " PRIMARY KEY"))]))

(defn gen-column
  "Generates a Column with the given name."
  [name]
  (->> (g/hash-map :name (g/return name)
                   :type (g/elements all-types-vec)
                   :primary-key? g/boolean)
       (g/fmap map->Column)))

(defrecord Table
  [name cols]
  DDL
  (setup [this]
    [(splice*
       (conj ["CREATE TABLE " name " ("]
             (splice* ", " (map sql cols))
             ")"))])

  (teardown [_]
    [(str "DROP TABLE " name)]))

(defn remove-duplicate-primary-keys
  "Takes a vector of columns and limits them to just one primary key."
  [cols]
  (loopr [pk?   false
          cols' []]
         [col cols]
         (let [pk?' (or pk? (:primary-key? col))]
           (recur pk?'
                  (conj cols'
                        (if (and pk? pk?')
                          (assoc col :primary-key? false)
                          col))))
         cols'))

(defn gen-table
  "Generates a Table. Options are:

      :max-column-count The maximum number of columns"
  [opts name]
  ; We're doing a weird dance here to miminize the recurrent use of
  ; bind--this should hopefully shrink better.
  (let [mcc (:max-column-count opts 6)]
    (->> (g/tuple (g/choose 1 mcc)
                  (g/shuffle (vec (take mcc column-names)))
                  (probability 1/100))
         ; Generate columns
         (bind (fn [[col-count col-names duplicate-pks?]]
                 (g/tuple
                   (->> col-names
                        (take col-count)
                        (mapv gen-column)
                        (apply g/tuple))
                   (g/return duplicate-pks?))))
         ; Filter columns and produce a schema
         (g/fmap (fn [[cols duplicate-pks?]]
                   (let [cols (if duplicate-pks?
                                cols
                                (remove-duplicate-primary-keys cols))]
                     (map->Table {:name name
                                  :cols cols})))))))

(defrecord Schema
  [; A vector of Tables
   tables
   ; A map of types to a handful of values, like {:integer [3, 4]}. We use this
   ; to generate similar values often in a given schema, which makes joins and
   ; equality predicates more likely to match.
   vpool]

  DDL
  (setup [_]
    (reduce into (map setup tables)))
  (teardown [_]
    (reduce into (map teardown tables))))

(defn gen-schema
  "Generator for a Schema. Options are:

      :max-table-count  The maximum number of tables
      :max-column-count The maximum number of columns"
  [opts]
  (->> (g/tuple
         (g/choose 1 (:max-table-count opts (count table-names)))
         (g/shuffle table-names)
         (g/return {}))
       (bind (fn [[table-count table-names vpool]]
               (g/hash-map
                 :tables (->> table-names
                              (take table-count)
                              (mapv (partial gen-table opts))
                              (apply g/tuple))
                 :vpool (g/return vpool))))
       (g/fmap map->Schema)))

; Represents an SQL parameterized literal value like 2 or 'hi'.
(defrecord Literal [x]
  SQL
  (sql [_]
    ["?" x]))

(defn gen-lit
  "Generates a literal of the given type."
  [opts schema type]
  (case type
    :int (gen-long Integer/MIN_VALUE Integer/MAX_VALUE)))

; Represents a column name like `age`
(defrecord ColumnName [name]
  SQL
  (sql [_]
    name))

; Represents an equality comparison like `name = 'regina'`
(defrecord Equals [lhs rhs]
  SQL
  (sql [_]
    (splice (sql lhs) " " (sql rhs))))

(defrecord Insert [table cols values]
  SQL
  (sql [_]
    (into [(str "INSERT INTO " table "(" (str/join ", " cols)
                ") VALUES (" (str/join ", " (repeat (count values) "?")) ")")]
          values)))

(defn gen-insert
  "Represents an INSERT statement."
  [opts schema]
  (g/let [table (g/elements (:tables schema))
          cols  (g/shuffle (:cols table))
          vals  (->> cols
                     (mapv (comp (partial gen-lit opts schema) :type))
                     (apply g/tuple))]
    (Insert. (:name table)
             (mapv :name cols) vals)))

; Represents a select statement like `select * from people where name =
; regina`.
(defrecord Select [table-name predicate]
  SQL
  (sql [_]
    (splice "SELECT * FROM "
            table-name
            (when predicate
              (splice " WHERE " (sql predicate))))))

(defn gen-select
  "Generator of select statements"
  [opts schema]
  (->> (g/elements (:tables schema))
       (g/fmap (fn [table]
                 (Select. (:name table) nil)))))

(defn gen-statement
  "Generator of statements."
  [opts schema]
  (g/frequency [[8 (gen-select opts schema)]
                [8 (gen-insert opts schema)]]))

(defrecord Case [schema statements]
  DDL
  (setup [_] (setup schema))
  (teardown [_] (teardown schema)))

(defn gen-case
  "Generator for a Case. Options:

      :max-table-count      The maximum number of tables
      :max-column-count     The maximum number of columns
      :max-statement-count  The maximum number of statements"
  [opts]
  (g/let [schema (gen-schema opts)
          statements (g/vector (gen-statement opts schema)
                               1 (:max-statement-count opts 64))]
    (Case. schema statements)))

(defn generate
  "Generates a Case given options. Options are:

      :seed                 The random seed
      :size                 The size parameter for generation
      :max-table-count      The maximum number of tables
      :max-column-count     The maximum number of columns
      :max-statement-count  The maximum number of statements"
  [opts]
  (let [size (:size opts 32)
        seed (:seed opts (rand/long))]
    (g/generate (gen-case opts) size seed)))
