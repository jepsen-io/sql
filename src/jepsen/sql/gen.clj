(ns jepsen.sql.gen
  "Test.check generators for simple schemas and statements."
  (:require [clojure [core :as c]
                     [math :refer [round]]
                     [pprint :refer [pprint]]
                     [set :as set]
                     [string :as str]
                     [walk :refer [prewalk]]]
            [clojure.test.check :as tc]
            [clojure.test.check [generators :as g]]
            [clojure.tools.logging :refer [info warn]]
            [jepsen [random :as rand]]
            [jepsen.sql [ast :refer :all]]
            [dom-top.core :refer [loopr]])
  (:import (jepsen.sql.ast Case
                           ColumnName
                           Equals
                           Literal
                           Select
                           Schema
                           TableName
                           Update)))

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

(def all-types
  "All possible types, as a set."
  (set all-types-vec))

(def num-types
  "All numeric types, as a set."
  #{:smallint :int :bigint :real :double})

(def text-types
  "All textual types, as a set."
  #{:character3 :character-varying3 :text})

(defn general-type
  "Takes a type and returns either a set of compatible types, or the type
  itself. We use this to generate (e.g.) assignments from float to int."
  [type]
  (case type
    :smallint           num-types
    :int                num-types
    :bigint             num-types
    :real               num-types
    :double             num-types
    :character3         text-types
    :character-varying3 text-types
    :text               text-types
    type))

(defn gen-type
  "A generator for a type valid in the given table."
  [opts schema table]
  (->> (:cols table)
       (map :type)
       distinct
       vec
       g/elements
       ; Right now we're going to limit ourselves to generating *exactly*
       ; the available types, because I don't trust myself to robustly
       ; implement all the details of float/decimal/integer coercion.
       ;(g/fmap general-type)
       ))

; Columns, Tables, Schemas

(defn gen-column
  "Generates a Column with the given name."
  [name]
  (->> (g/hash-map :name (g/return name)
                   :type (g/elements all-types-vec)
                   :primary-key? g/boolean)
       (g/fmap map->Column)))

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
  (let [mcc (:max-column-count opts 4)]
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

;; Literals

(defn gen-lit*
  "Generates a Literal of the given type."
  [opts schema type]
  (if (set? type)
    (g/one-of (mapv (partial gen-lit* opts schema) type))
    (->> (case type
           :int (gen-long Integer/MIN_VALUE Integer/MAX_VALUE))
         (g/fmap ->Literal))))

(defn gen-lit
  "Generates a Literal of the givne type, sometimes picking dense values from
  the schema's vpool."
  [opts schema type]
  (gen-lit* opts schema type))

;; Expressions

(defn gen-column-name
  "Generates a ColumnName of the given type in the given table. Returns nil
  rateher than a generator if there are no columns of this type."
  [opts schema table type]
  (let [cols (->> (:cols table)
                  (filter (if (keyword? type)
                            (comp #{type} :type)
                            (comp type :type)))
                  (mapv column-name))]
    (when (seq cols)
      (g/elements cols))))

(defn gen-expr-leaf
  "A generator which returns simple expressions of the given type, like
  Literals or ColumnNames."
  [opts schema table type]
  (if-let [col (gen-column-name opts schema table type)]
    (g/one-of [(gen-lit opts schema type) col])
    ; No valid columns, just do a literal.
    (gen-lit opts schema type)))

(defn gen-expr
  "Takes options, schema, a table, a type, and a depth (which controls nested
  expressions). Returns a generator of expressions of the given type. Depth 0
  means only bare literals."
  ([opts schema table type depth]
   (condp = type
     ; For any other type, we only generate leaves
     (gen-expr-leaf opts schema table type))))

(defn gen-expr-same-type
  "A generator which picks a type in the table and constructs expressions which
  are all of that type, up to the given recursion depth."
  [opts schema table depth]
  (g/bind (gen-type opts schema table)
          (fn [type]
            (gen-expr opts schema table type depth))))

;; Predicates

(defn gen-equals
  "Generates an Equals expression, given a generator of sub-expressions."
  [expr-gen]
  (flet [left  expr-gen
         right expr-gen]
        (Equals. left right)))

(defn gen-expr-boolean
  "Generates a boolean expression of the given depth."
  ([opts schema table]
   (gen-expr-boolean opts schema table 3))
  ([opts schema table depth]
   (gen-equals
     (gen-expr-same-type opts schema table (dec depth)))))

;; Statements

(defn gen-where
  "Generates a (possibly nil) WHERE clause."
  [opts schema table]
  (g/frequency
    [[1 (g/return nil)]
     [3 (gen-expr-boolean opts schema table)]]))

(defn gen-insert
  "Generator of Inserts"
  [opts schema]
  (g/let [table (g/elements (:tables schema))
          cols  (g/shuffle (:cols table))
          vals  (->> cols
                     (mapv (comp (partial gen-lit opts schema) :type))
                     (apply g/tuple))]
    (insert (TableName. (:name table))
            (mapv column-name cols)
            vals)))

(defn gen-update-pair
  "A generator which produces a [col-name value-expr] pair for an UPDATE"
  [opts schema table]
  (g/let [col (g/elements (:cols table))
          value (gen-lit opts schema (:type col))]
    [(ColumnName. (:name col)) value]))

(defn gen-update
  "Generator of Updates."
  [opts schema]
  (g/let [table (g/elements (:tables schema))
          set (let [n (count (:cols table))]
                 ; Without duplicates
                 (g/vector-distinct-by first
                                       (gen-update-pair opts schema table)
                                       {:min-elements 1, :max-elements n}))
          where (gen-where opts schema table)]
    (Update. (TableName. (:name table)) set where)))

(defn gen-select
  "Generator of select statements"
  [opts schema]
  (->> (g/elements (:tables schema))
       (bind (fn [table]
               (flet [where (gen-where opts schema table)]
                 (Select. (TableName. (:name table)) where))))))

(defn gen-statement
  "Generator of statements."
  [opts schema]
  (g/frequency [[8 (gen-select opts schema)]
                [8 (gen-insert opts schema)]
                [4 (gen-update opts schema)]]))

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
