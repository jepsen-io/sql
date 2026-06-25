(ns jepsen.sql.ast
  "An abstract syntax tree representation of basic SQL schemas and statements."
  (:refer-clojure :exclude [eval update])
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

(defprotocol Eval
  (eval-without-row? [this]
                     "Can this be evaluated without a row?")

  (eval [this row]
        "Evaluates an expression in the context of a row. A row is a map of
        keyword local column names to values, like `{:name \"streisand\",
        :effect_power 42}`"))

(defprotocol SQL
  (sql [this] "Converts this AST node to an SQL vector suitable for use with
              `j/execute!"))

(defprotocol DDL
  (setup [this]
    "A vector of SQL statements that will create this thing.")
  (teardown [this]
    "A vector of SQL statements that will tear down this thing."))

(defn splice
  "Splices n SQL vectors or strings together. We do this by concatenating
  their strings and remaining parameters. Nil vanishes."
  ([a]
   (if (string? a)
     [a]
     a))
  ([a b]
   (cond (nil? a) (splice b)
         (nil? b) (splice a)
         true (if (string? a)
                (if (string? b)
                  [(str a b)]
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
    ["?" x])

  Eval
  (eval-without-row? [_] true)
  (eval [_ row] x))

(defn literal
  "Constructs a Literal."
  [x]
  (assert (not (satisfies? SQL x)))
  (Literal. x))

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

(defn column
  "Takes a name, type, and a map of options; constructs a Column."
  ([name type]
   (assert (string? name))
   (Column. name type false))
  ([name type opts]
   (assert (string? name))
   (map->Column (assoc opts :name name :type type))))

; Represents a column name like `age`. We use these to represent columns in
; statements, to distinguish them from literals.
(defrecord ColumnName [name]
  SQL
  (sql [_] name)
  Eval
  (eval-without-row? [_] false)
  (eval [_ row]
    (let [r (get row (keyword name) ::not-found)]
      (when (identical? ::not-found r)
        (throw (IllegalArgumentException.
                 (str "Row " (pr-str row) " has no column "
                      (pr-str (keyword name))))))
      r)))

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

(defn table
  "Constructs a Table, given a name and a vector of Columns."
  [name cols]
  (assert (string? name))
  (assert (every? (partial instance? Column) cols))
  (Table. name cols))

; Represents a table name like 't'
(defrecord TableName [name]
  SQL
  (sql [_] name))

(defn table-name
  "Constructs a TableName from either a Column or a string."
  [table]
  (cond (string? table)
        (TableName. table)

        (instance? Table table)
        (TableName. (:name table))

        true
        (throw (IllegalArgumentException.
                 "Expected a string column name or a Column"))))

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

(defn schema
  "Constructs a schema from a vector of tables, and optionally a vpool."
  ([tables]
   (assert (vector? tables))
   (assert (every? (partial instance? Table) tables))
   (Schema. tables {})))

(defrecord Insert [table cols values]
  SQL
  (sql [_]
    (splice "INSERT INTO " (sql table) " ("
            (splice* ", " (map sql cols))
            ") VALUES ("
            (splice* ", " (map sql values))
            ")")))

;; Expressions

; Represents an equality comparison like `name = 'regina'
(defrecord Equals [left right]
  SQL
  (sql [_]
    (splice "(" (sql left) " = " (sql right) ")"))

  Eval
  (eval-without-row? [_]
    (and (eval-without-row? left)
         (eval-without-row? right)))

  (eval [_ row]
    (= (eval left row) (eval right row))))

(defn equals
  "Constructs an Equals expression."
  [left right]
  (assert (satisfies? SQL left))
  (assert (satisfies? SQL right))
  (Equals. left right))

; Represents a (NOT A) expression
(defrecord Not [expr]
  SQL
  (sql [_]
    (splice "(NOT " (sql expr) ")"))

  Eval
  (eval-without-row? [_]
    (eval-without-row? expr))

  (eval [_ row]
    (not (eval expr row))))

; Represents (A AND B AND ...)
(defrecord And [exprs]
  SQL
  (sql [_]
    (splice "("
            (splice* " AND " (map sql exprs))
            ")"))

  Eval
  (eval-without-row? [_]
    (every? eval-without-row? exprs))

  (eval [_ row]
    (loop [exprs exprs]
      (if (seq exprs)
        (let [x (eval (first exprs) row)]
          (assert (boolean? x))
          (if x
            (recur (next exprs))
            false))
        ; No more expressions to check
        true))))

; Represents (A OR B OR ...)
(defrecord Or [exprs]
  SQL
  (sql [_]
    (splice "(" (splice* " OR " (map sql exprs)) ")"))

  Eval
  (eval-without-row? [_]
    (every? eval-without-row? exprs))

  (eval [_ row]
    (loop [exprs exprs]
      (if (seq exprs)
        (let [x (eval (first exprs) row)]
          (assert (boolean? x))
          (if x
            true
            (recur (next exprs))))
        false))))

(defn row->pred
  "Turns a row map into a predicate which matches that row, or any row with
  additional fields."
  [m]
  (And. (mapv (fn [[k v]]
                (Equals. (column-name (name k)) (literal v)))
              m)))

(defn column-names-in
  "Takes an SQL expression and collects a set of all ColumnNames in it."
  [expr]
  (let [cols (volatile! (transient #{}))]
    (walk/prewalk
      (fn search [x]
        (when (instance? ColumnName x)
          (vswap! cols conj! x)
          x))
      expr)
    (persistent! @cols)))

;; Statements

(defn insert
  "Constructs an Insert."
  [table cols values]
  (assert (every? (partial instance? ColumnName) cols))
  (assert (every? (partial satisfies? SQL) values))
  (Insert. (table-name table) cols values))


; An UPDATE statement like "UPDATE cats SET age = age + 1 WHERE name =
; 'professor meowington'
(defrecord Update [table ; A TableName
                   set   ; A vector of [col-name val-expr] pairs
                   where ; A predicate expression
                   ]
  SQL
  (sql [_]
    (splice "UPDATE " (sql table) " SET "
            (splice* ", "
                     (map (fn [[col expr]]
                            (splice (sql col) " = " (sql expr)))
                          set))
            (when where
              (splice " WHERE " (sql where))))))

(defn update
  "Constructs an Update."
  [table set where]
  (assert (vector? set))
  (assert (every? (partial instance? ColumnName) (map first set)))
  (assert (every? (partial satisfies? SQL) (map second set)))
  (assert (or (nil? where)
              (satisfies? SQL where)))
  (Update. (table-name table) set where))

; A DELETE statement like "DELETE FROM treats WHERE tasty FALSE"
(defrecord Delete [table ; A TableName
                   where ; A predicate expression
                   ]
  SQL
  (sql [_]
    (splice "DELETE FROM " (sql table)
            (when where
              (splice " WHERE " (sql where))))))

(defn delete
  "Constructs a Delete."
  [table where]
  (assert (or (nil? where) (satisfies? SQL where)))
  (Delete. (table-name table) where))

; Represents a select statement like `select * from people where name =
; regina`.
(defrecord Select [table where]
  SQL
  (sql [_]
    (splice ["SELECT * FROM "]
            (sql table)
            (when where
              (splice " WHERE " (sql where))))))

(defn select
  "Constructs a Select."
  [table where]
  (assert (or (nil? where) (satisfies? SQL where)))
  (Select. (table-name table) where))

; A Case is a schema and a vector of statements.
(defrecord Case [schema statements]
  DDL
  (setup    [_] (setup schema))
  (teardown [_] (teardown schema)))

(defn rewrite-tables
  "Rewrites all table names in a case using a function (f table-name-str)."
  [case f]
  (walk/prewalk
    (fn rewrite [x]
      ; Ugh, this is going to break on hot code reloading because we define a
      ; new class for Table etc. with every defrecord, and they won't match
      (condp instance? x
        Table     (c/update x :name f)
        TableName (c/update x :name f)
        x))
    case))

(defn unique-tables
  "Rewrites all tables in a case to have a unique suffix, like t_1"
  [case i]
  (rewrite-tables case #(str % "_" i)))
