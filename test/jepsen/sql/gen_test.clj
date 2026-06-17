(ns jepsen.sql.gen-test
  (:require [clojure [pprint :refer [pprint]]
                     [test :refer :all]]
            [clojure.test.check.generators :as g]
            [jepsen [random :as rand]]
            [jepsen.sql [gen :refer :all]]))

(deftest gen-schema-test
  (is (= (->Schema
           [(->Table "t"
                     [(map->Column {:name "c" :type :int, :primary-key? true})
                      (map->Column {:name "b", :type :int, :primary-key? false})
                      (map->Column {:name "a", :type :int, :primary-key? false})])
            (->Table "u"
                     [(map->Column {:name "b", :type :int, :primary-key? true})])]
           {})
         (g/generate (gen-schema {:max-table-count  3
                                  :max-column-count 3})
                     2 123))))

#_(deftest gen-case-test
  (rand/with-seed 0
    (let [case (generate {})]
      (mapv prn (setup case))
      (mapv (comp prn sql) (:statements case))
      (mapv prn (teardown case)))))

