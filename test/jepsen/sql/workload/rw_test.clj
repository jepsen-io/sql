(ns jepsen.sql.workload.rw-test
  "A test for the rw workload on Postgres."
  (:require [clj-commons.slingshot :refer [try+ throw+]]
            [clojure [pprint :refer [pprint]]
                     [set :as set]
                     [string :as str]
                     [test :refer :all]]
            [clojure.tools.logging :refer [info warn]]
            [jepsen.sql.base-test :refer :all]))


(deftest rw-test
  (let [test' (run-workload! {:workload :internal
                              :isolation :read-uncommitted})
        res (:internal (:results test'))]
    (is (false? (:valid? res)))
    (let [e (first (:errors res))]
      (is (map? (:op e)))
      (is (vector? (:mop e)))
      (is (integer? (:k e))))
    (is (pos? (:error-count res)))
    (is (pos? (:txn-count res)))))
