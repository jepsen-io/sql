(ns jepsen.sql.workload.internal-sim-test
  "A test for the internal-sim workload on Postgres."
  (:require [clj-commons.slingshot :refer [try+ throw+]]
            [clojure [pprint :refer [pprint]]
                     [set :as set]
                     [string :as str]
                     [test :refer :all]]
            [clojure.tools.logging :refer [info warn]]
            [jepsen.sql.base-test :refer :all]))

(deftest ^:focus internal-sim-test
  (let [test' (run-workload! {:log-sql   true
                              :workload  :internal-sim
                              :isolation :read-uncommitted})
        res (:internal (:results test'))]
    (is (false? (:valid? res)))
    (let [e (first (:errors res))]
      (is (integer? (:case e)))
      (is (#{:missing-rows} (:type e)))
      (is (vector? (:statement e))))
    (is (pos? (:error-count res)))
    (is (pos? (:txn-count res)))))

(deftest internal-sim-test-serializable
  (let [test' (run-workload! {:workload :internal-sim
                              :isolation :serializable})
        res (:internal (:results test'))]
    (pprint res)
    (is (true? (:valid? res)))))
