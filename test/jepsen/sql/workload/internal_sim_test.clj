(ns jepsen.sql.workload.internal-sim-test
  "A test for the internal-sim workload on Postgres."
  (:require [clj-commons.slingshot :refer [try+ throw+]]
            [clojure [pprint :refer [pprint]]
                     [set :as set]
                     [string :as str]
                     [test :refer :all]]
            [clojure.tools.logging :refer [info warn]]
            [jepsen.sql.base-test :refer :all]))

(defn valid-row
  "Does a row look all right?"
  [row]
  (doseq [[col val] row]
    (is (keyword? col))))

(defn valid-missing-rows
  [{:keys [statement missing expected actual]}]
  (is (vector? statement))
  (is (set? missing))
  ; At least one thing should be missing
  (is (seq missing))
  (is (set? expected))
  (is (set/superset? expected missing))
  ; All rows should be well-formed
  (mapv valid-row missing)
  (mapv valid-row expected))

(defn valid-error
  [{:keys [index txn statement] :as err}]
  (is (integer? index))
  (is (integer? (:case err)))
  (is (vector? txn))
  (is (every? (fn [mop]
                (and (= #{:statement :results} (set (keys mop)))
                     (vector? (:statement mop))))
              txn))
  (is (vector? statement))
  (is (string? (first statement)))
  (case (:type err)
    :missing-rows (valid-missing-rows err)))

(deftest internal-sim-test
  (let [test' (run-workload! {:log-sql   true
                              :workload  :internal-sim
                              :isolation :read-uncommitted})
        res (:internal (:results test'))]
    (is (false? (:valid? res)))
    (is (pos? (:error-count res)))
    (is (pos? (:txn-count res)))
    (let [e (first (:errors res))]
      (valid-error e))))

(deftest internal-sim-test-serializable
  (let [test' (run-workload! {:workload :internal-sim
                              :isolation :serializable})
        res (:internal (:results test'))]
    (is (true? (:valid? res)))))
