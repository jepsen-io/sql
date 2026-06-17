(ns jepsen.sql.ast
  "An abstract syntax tree representation of basic SQL schemas and statements."
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

; Represents an SQL parameterized literal value like 2 or 'hi'.
(defrecord Literal [x]
  SQL
  (sql [_]
    ["?" x]))

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

; Represents a column used as a part of a table.
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

; Represents an SQL table schema
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

; A Schema defines a collection of tables, and also some common values you
; might use to work with them.
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

(defrecord Insert [table cols values]
  SQL
  (sql [_]
    (into [(str "INSERT INTO " table "(" (str/join ", " cols)
                ") VALUES (" (str/join ", " (repeat (count values) "?")) ")")]
          values)))

; Represents a select statement like `select * from people where name =
; regina`.
(defrecord Select [table-name predicate]
  SQL
  (sql [_]
    (splice ["SELECT * FROM "]
            table-name
            (when predicate
              (splice " WHERE " (sql predicate))))))

; A Case is a schema and a vector of statements.
(defrecord Case [schema statements]
  DDL
  (setup [_] (setup schema))
  (teardown [_] (teardown schema)))
