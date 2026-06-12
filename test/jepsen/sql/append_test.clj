(ns jepsen.sql.append-test
  "A test for the list-append workload on Postgres."
  (:require [clj-commons.slingshot :refer [try+ throw+]]
            [clojure [pprint :refer [pprint]]
                     [set :as set]
                     [string :as str]
                     [test :refer :all]]
            [clojure.tools.logging :refer [info warn]]
            [jepsen.sql.base-test :refer :all]))

(deftest append-test
  (let [test' (run-workload! {:workload                   :append
                              :isolation                  :read-uncommitted
                              :expected-consistency-model :serializable})
        res (:append (:results test'))]
    (is (false? (:valid? res)))
    ; (pprint (:results test'))
    (is (set/superset? (set (:anomaly-types res))
                       #{:internal :lost-update :G2-item}))))


