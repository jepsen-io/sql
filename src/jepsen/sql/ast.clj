(ns jepsen.sql.ast
  "An abstract syntax tree representation of basic SQL schemas and statements."
  (:require [clojure [core :as c]
                     [math :refer [round]]
                     [pprint :refer [pprint]]
                     [set :as set]
                     [string :as str]
                     [walk :as walk]]
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
    "A vector of SQL statements that will create this thing.")
  (teardown [this]
    "A vector of SQL statements that will tear dwon this thing."))

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

; A general-purpose box for any SQL vector
(defrecord Statement [sql]
  SQL
  (sql [_] sql))

; Represents an SQL parameterized literal value like 2 or 'hi'.
(defrecord Literal [x]
  SQL
  (sql [_]
    ["?" x]))

; Represents a table name like 't'
(defrecord TableName [name]
  SQL
  (sql [_] name))

; Represents an equality comparison like `name = 'regina'
(defrecord Equals [left right]
  SQL
  (sql [_]
    (splice "(" (sql left) " = " (sql right) ")")))

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

; Represents a column name like `age`. We use these to represent columns in
; statements, to distinguish them from literals.
(defrecord ColumnName [name]
  SQL
  (sql [_] name))

(defn column-name
  "Constructs a ColumnName from either a Column or a string."
  [col]
  (cond (string? col)
        (ColumnName. col)

        (instance? Column col)
        (ColumnName. (:name col))

        true
        (throw (IllegalArgumentException.
                 "Expected a string column name or a Column"))))

; Represents an SQL table schema
(defrecord Table
  [name cols]
  DDL
  (setup [this]
    [(Statement.
      (splice*
         (conj ["CREATE TABLE " name " ("]
               (splice* ", " (map sql cols))
               ")")))])

  (teardown [_]
    [(Statement. [(str "DROP TABLE " name)])]))

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
    (reduce into [] (map setup tables)))
  (teardown [_]
    (reduce into [] (map teardown tables))))

(defrecord Insert [table cols values]
  SQL
  (sql [_]
    (splice "INSERT INTO " (sql table) " ("
            (splice* ", " (map sql cols))
            ") VALUES ("
            (splice* ", " (map sql values))
            ")")))

(defn insert
  "Constructs an Insert."
  [table cols values]
  (assert (instance? TableName table))
  (assert (every? (partial instance? ColumnName) cols))
  (assert (every? (partial satisfies? SQL) values))
  (Insert. table cols values))


; An UPDATE statement like "UPDATE cats SET age = age + 1 WHERE name =
; 'professor meowington'
(defrecord Update [table ; A TableName
                   set   ; A vector of [col-name val-expr] pairs
                   where ; A predicate expression
                   ]
  SQL
  (sql [_]
    (splice [(str "UPDATE " (sql table) " SET ")]
            (splice* ", "
                     (map (fn [[col expr]]
                            (splice (sql col) " = " (sql expr)))
                          set))
            (when where
              (splice " WHERE " (sql where))))))


; Represents a select statement like `select * from people where name =
; regina`.
(defrecord Select [table where]
  SQL
  (sql [_]
    (splice ["SELECT * FROM "]
            (sql table)
            (when where
              (splice " WHERE " (sql where))))))

; A Case is a schema and a vector of statements.
(defrecord Case [schema statements]
  DDL
  (setup    [_] (setup schema))
  (teardown [_] (teardown schema)))

(defn unique-tables
  "Rewrites all tables in a case to have a unique suffix, like t_1"
  [case i]
  (walk/prewalk
    (fn rewrite [x]
      ; Ugh, this is going to break on hot code reloading because we define a
      ; new class for Table etc. with every defrecord, and they won't match
      (condp instance? x
        Table     (update x :name str "_" i)
        TableName (update x :name str "_" i)
        x))
    case))
