(ns jepsen.sql.checker
  "There are a few common things that can go wrong in an SQL test. We encode
  these as :type :info operations with a special :error like

    {:type      :jepsen.sql/wrong-type
     :expected 'java.lang.Integer
     :actual   'java.lang.String}

  We offer a checker which looks for these errors. This checker is
  automatically composed with each workload checker."
  (:require [jepsen [checker :as checker]
                    [history :as h]]
            [tesser [core :as t]]
            [clj-commons.slingshot :refer [try+ throw+]]))

(defrecord SQLChecker []
  checker/Checker
  (check [this test history opts]
    (let [errs (->> (t/filter (fn filter [op]
                                (when (h/info? op)
                                  (when-let [e (:type (:error op))]
                                    (when (= "jepsen.sql" (namespace e))
                                      true)))))
                    (t/group-by :type)
                    (t/into [])
                    (h/tesser history))]
      {:valid?      (not (seq errs))
       :error-types (sort (keys errs))
       :errors      errs})))

(defn compose
  "Takes an existing workload checker and composes it with our SQL checker,
  naming the original checker the given key."
  [checker k]
  (checker/compose
    {k     checker
     :sql  (SQLChecker.)}))

;; Some common assertions that will trigger our checker

(defn assert-at-most-one
  "Asserts that there's at most one thing in a collection. Returns coll. Takes
  a map of additional keys to merge into thrown errors."
  ([coll]
   (assert-at-most-one coll nil))
  ([coll m]
   (when (< 1 (count coll))
     (throw+ (merge {:type :jepsen.sql/too-many
                     :coll coll}
                    m)))
   coll))

(defn assert-instance-or-nil
  "Asserts that x is of the given type, or nil. Returns x. Takes a map of
  additional keys to merge into thrown errors."
  ([type x]
   (assert-instance-or-nil type x nil))
  ([type x m]
   (when-not (or (nil? x) (instance? type x))
     (throw+ (merge {:type      :jepsen.sql/wrong-type
                     :expected  (symbol (.getName type))
                     :actual    (symbol (.getName (class x)))
                     :value     x})))
   x))
