(ns jepsen.sql.workload.internal-sim
  "A more complex test for internal consistency. Generates random schemas and
  transactions over them. For each transaction, verifies that the operations
  within that transaction appear to execute in isolation. This is not
  particularly smart; we build up a partial set of known-extant rows over time
  and flag places where a row should have appeared but did not. This will not
  catch things like 'I deleted everything then read a row'."
  (:refer-clojure :exclude [read])
  (:require [clojure.tools.logging :refer [info warn]]
            [clojure [pprint :refer [pprint]]
                     [set :as set]
                     [string :as str]]
            [dom-top.core :refer [loopr with-retry]]
            [elle.core :as elle]
            [jepsen [checker]
                    [client :as client]
                    [core :as jepsen]
                    [generator :as gen]
                    [history :as h]
                    [random :as rand]
                    [util :as util]]
            [jepsen.sql [client :as c]
                        [checker :as checker :refer [assert-at-most-one
                                                     assert-instance-or-nil]]
                        [encoding :as encoding]
                        [ast :refer :all]
                        [gen :as sql.gen]]
            [next.jdbc :as j]
            [next.jdbc.result-set :as rs]
            [next.jdbc.sql.builder :as sqlb]
            [clj-commons.slingshot :refer [try+ throw+]]
            [tesser.core :as t])
  (:import (jepsen.sql.ast ColumnName
                           Delete
                           Equals
                           Insert
                           Literal
                           Select
                           Statement
                           Update)))

(defprotocol Expr
  (eval-expr [node row]
             "Evaluates an AST expression in the context of a given row."))

(extend-protocol Expr
  ; Column names are looked up in the row.
  ColumnName
  (eval-expr [this row]
    (get row (keyword (:name this)) :unknown))

  ; Literals are themselves
  Literal
  (eval-expr [this row]
    (:x this))

  Equals
  (eval-expr [this row]
    (= (eval-expr (:left this) row)
       (eval-expr (:right this) row))))

(defn update-row
  "Takes a row and a series of [column expr] pairs. Returns the row with each
  column set to expr."
  [row pairs]
  (persistent!
    (reduce (fn [row [col expr]]
              (assoc! row (keyword (:name col)) (eval-expr expr row)))
            (transient row)
            pairs)))


(defrecord Table [pkey-fn sort-fn rows])

(defrecord State [tables])

(defn initial-state
  "Computes an initial State for a schema."
  [schema]
  (->> (:tables schema)
       (map (fn [table]
              (let [pkeys (->> (:cols table)
                               (filter :primary-key?)
                               (map :name)
                               (map keyword))
                    pkey-fn (case (count pkeys)
                              0 nil
                              1 (first pkeys)
                              (apply juxt pkeys))
                    cols (sort (map (comp keyword :name) (:cols table)))
                    sort-fn (fn [a b]
                              (loop [cols cols]
                                (if (seq cols)
                                  (let [col (first cols)
                                        c (compare
                                            (get a col)
                                            (get b col))]
                                    (if (= 0 c)
                                      (recur (next cols))
                                      c))
                                  ; No columns left; must be equal
                                  0)))]
                [(:name table)
                 (Table. pkey-fn sort-fn #{})])))
       (into (sorted-map))
       (State.)))

(defn sorted-rows
  "When returning errors, it's much nicer to sort. Takes a State, the string
  table name, and collection of rows, and makes a sorted set of those rows,
  each row a sorted map."
  [state table rows]
  (let [sort-fn (-> state :tables (get table) :sort-fn)]
    (->> rows
         (map (partial into (sorted-map)))
         (into (sorted-set-by sort-fn)))))

(defprotocol CheckStatement
  (check-statement [statement state results]
                   "Checks a single statement. Takes a statement, a State, and
                   the results of evaluating the statement. Returns either a
                   new State or an error map, if we can show the statement was
                   inconsistent with this State."))

(extend-protocol CheckStatement
  ; When we have an insert, we know its rows become a part of the state map.
  Insert
  (check-statement [statement state results]
    (if (map? results)
      ; Error
      state
      ; Success
      (let [table (:name (:table statement))
            row   (->> (:values statement)
                       ; Evaluate each value
                       (map (fn [expr]
                              (eval-expr expr nil)))
                       ; And zip together with columns
                       (zipmap (map (comp keyword :name) (:cols statement))))]
        (update-in state [:tables table :rows] conj row))))

    ; When we perform an update, we apply it to the rows we know about.
    Update
    (check-statement [statement state results]
      (if (map? results)
        ; Error
        state
        ; Success
        (let [table (:name (:table statement))
              where (:where statement)
              rows  (get-in state [:tables table :rows])]
          ; Work through each row, transforming it if it matches the predicate
          (loopr [rows'        rows
                  update-count 0]
                 [row rows]
                 (if (or (nil? where) (eval-expr where row))
                   (recur (-> rows'
                              (disj row)
                              (conj (update-row row (:set statement))))
                          (inc update-count))
                   (recur rows' update-count))
                 ; We know a subset of the DB, so would be bad if we updated
                 ; more rows than the database did!
                 (cond (< (:next.jdbc/update-count (first results))
                          update-count)
                       (reduced {:type      :not-enough-updated-rows
                                 :statement (sql statement)
                                 :expected  [:at-least update-count]
                                 :actual    (first results)
                                 :rows      (sorted-rows state table rows)})

                       ; OK, all set
                       true
                       (assoc-in state [:tables table :rows] rows'))))))

    ; When we delete something, we destroy every matching row in our set.
    Delete
    (check-statement [statement state results]
      (if (map? results)
        ; Error
        state
        ; Success
        (let [table (:name (:table statement))
              where (:where statement)
              rows  (get-in state [:tables table :row])]
          ; Delete each row matching the predicate
          (loopr [rows'         rows
                  update-count  0]
                 [row rows]
                 (if (or (nil? where) (eval-expr where row))
                   (recur (disj rows' row)
                          (inc update-count))
                   (recur rows' update-count))
                 ; We know a subset of the DB, so it would be bad if we deleted
                 ; more rows than the DB did.
                 (cond (< (:next.jdbc/update-count (first results))
                          update-count)
                       (reduced {:type :not-enough-updated-rows
                                 :statement (sql statement)
                                 :expected  [:at-least update-count]
                                 :actual    (first results)
                                 :rows      (sorted-rows state table rows)})
                       ; All set
                       true
                       (assoc-in state [:tables table :rows] rows'))))))

    ; When we have a select, we evaluate the predicate against our state and
    ; ensure that all the rows we think should be present are present.
    Select
    (check-statement [statement state results]
      (if (map? results)
        ; Error
        state
        ; Success
        (let [table (:name (:table statement))
              results (set results)
              ; What rows should be present?
              required (->> (get-in state [:tables table :rows])
                            (filter
                              (if-let [w (:where statement)]
                                (partial eval-expr w)
                                ; With no WHERE clause, you match everything
                                (constantly true)))
                            set)
              ; Ugh these should be multisets, ah well
              missing (set/difference required results)]
          (if (seq missing)
            (reduced {:type       :missing-rows
                      :statement  (sql statement)
                      :missing    (sorted-rows state table missing)
                      :expected   (sorted-rows state table required)
                      :actual     (sorted-rows state table results)})

            ; Success; we know these rows are present in the table
            (update-in state [:tables table :rows] set/union results))))))

(defn check-txn
  "Takes an initial State and a transaction, represented as a series of SQL AST
  nodes, and a vector of corresponding results. Returns nil if no anomalies
  detected, or an anomaly map if we can find something wrong."
  [initial-state sql results]
  ;(prn)
  ;(println "## Check txn")
  (let [r (reduce (fn [state [statement results]]
                    (check-statement statement state results))
                  initial-state
                  (mapv vector sql results))]
    ; More robust than checking for instance of State, with hot code reloading
    (when (:type r)
      r)))

(defn check-op
  "Takes a history, a map of case numbers to initial States, and a completion
  operation. Returns nil if no anomalies detected, or an anomaly map if we can
  find something wrong."
  [history initial-states complete]
  (let [invoke (h/invocation history complete)
        initial-state (get initial-states (:case invoke))]
    (when-let [err (check-txn initial-state (:value invoke) (:value complete))]
      (assoc err
             :case  (:case invoke)
             :index (:index complete)
             :txn (mapv (fn [statement results]
                          {:statement (sql statement)
                           :results   results})
                        (:value invoke) (:value complete))))))

;; Client

(defn statement!
  "Applies a single statement. Returns results. Results may also be a map,
  which denotes an error."
  [test conn statement]
  (Thread/sleep (rand/zipf (:mop-delay test)))
  (let [sql (sql statement)]
    (c/try+
      (j/execute! conn sql {:builder-fn rs/as-unqualified-lower-maps})
      (catch [:definite? true] e
        e))))

(defrecord Client []
  c/Client
  (open! [this test conn node]
    this)

  (setup! [_ test conn])

  (invoke! [_ test conn op]
    (c/with-txn test [t conn]
      (case (:f op)
        :create
        (assoc op
               :type :ok
               :value (->> (:value op)
                           setup
                           (mapv (partial statement! test t))))

        :txn
        (assoc op
             :type :ok
             :value (->> (:value op)
                         (mapv (partial statement! test t)))))))

  (teardown! [_ test conn])

  (close! [_ test]))

(def max-txn-length 16)

(defn partition-statements
  "Partitions a vector of statements into a vector of vectors of statements;
  each one intended for a single transaction."
  [test statements]
  (when (seq statements)
    (lazy-seq
      (let [; No sense in testing just a single statement
            n (+ 2 (rand/long
                     (- (:max-txn-length test max-txn-length)
                        2)))
            [txn more] (split-at n statements)]
        (cons txn (partition-statements test more))))))

(defn new-case-delay
  "Returns a Delay which will generate a new Case for case number i."
  [i]
  (-> (sql.gen/generate {:max-statement-count 4096
                         :max-table-count 1
                         :max-column-count 2})
      (unique-tables i)
      delay))

(defrecord Generator [; Case number
                      i
                      ; Vector of transactions remaining
                      txns
                      ; This is a Delay which generates the next case. We wrap
                      ; this so we don't call it repeatedly and throw away
                      ; work.
                      case-delay]
  gen/Generator
  (op [this test ctx]
    (if-let [txn (first txns)]
      ; Keep issuing transactions
      [(gen/fill-in-op {:f :txn, :value txn, :case i} ctx)
       (Generator. i (next txns) case-delay)]
      ; Out of txns; generate a fresh case.
      (let [i' (inc i)
            case @case-delay
            txns (partition-statements test (:statements case))]
        ; One txn to create the tables, then more for the actual statements.
        [(gen/fill-in-op {:f :create, :value (:schema case), :case i'} ctx)
         (Generator. i' txns (new-case-delay (inc i')))])))

  (update [this test ctx event]
    this))

(defn gen
  "Generates a schema, then a series of transactions on it."
  []
  (Generator. -1 nil (new-case-delay 0)))

(defn internal-anomaly
  "Does an operation have an internal anomaly? If so, returns a map explaining
  it."
  [op]
  (:internal-anomaly op))

(defrecord Checker []
  jepsen.checker/Checker
  (check [this test history opts]
    (h/ensure-pair-index history)
    (let [; First pass: extract the schema for each case and convert it to an
          ; initial state.
          schemas (->> (t/filter (h/has-f? :create))
                       (t/filter h/invoke?)
                       (t/map (juxt :case :value))
                       (t/into {})
                       (h/tesser history))
          initial-states (util/map-vals initial-state schemas)

          ; Second pass: validate each txn
          {:keys [n errors]}
          (->> (t/filter h/ok?)
               (t/filter (h/has-f? :txn))
               (t/fuse
                 {:n      (t/count)
                  :errors (->> (t/keep (partial check-op
                                                history initial-states))
                               (t/into []))})
               (h/tesser history))]
      {:valid? (not (seq errors))
       :errors errors
       :error-count (count errors)
       :txn-count n})))

(defn workload
  "A workload for an internal consistency test. Takes CLI opts."
  [opts]
  {:generator (gen)
   :client    (c/client (Client.) opts)
   :checker   (checker/compose (Checker.) :internal)})
