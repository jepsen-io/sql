(ns jepsen.sql.gen-test
  (:require [clojure [pprint :refer [pprint]]
                     [test :refer :all]]
            [clojure.test.check.generators :as g]
            [jepsen [random :as rand]]
            [jepsen.sql [ast :refer :all]
                        [gen :refer :all]]))

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

(deftest gen-case-test
  (rand/with-seed 0
    (let [case (generate {})]
      ;(mapv (comp prn sql) (:statements case))
      (is (= [["CREATE TABLE t (c int PRIMARY KEY, a int, d int, b int, e int, f int)"]
              ["CREATE TABLE v (b int, f int PRIMARY KEY, a int, c int, e int, d int)"]
              ["CREATE TABLE u (f int)"]]
             (setup case)))
      (is (= ["DROP TABLE t" "DROP TABLE v" "DROP TABLE u"]
             (teardown case)))
      (is (= [["SELECT * FROM u"]
              ["SELECT * FROM v"]
              ["INSERT INTO v(b, c, e, f, a, d) VALUES (?, ?, ?, ?, ?, ?)"
               -2703
               8102
               -3
               -293844
               -100218
               173829]
              ["SELECT * FROM u"]
              ["SELECT * FROM u"]
              ["INSERT INTO t(c, a, d, b, e, f) VALUES (?, ?, ?, ?, ?, ?)"
               -3
               52
               1235
               3802968
               -3782491
               -1806915]
              ["SELECT * FROM t"]
              ["SELECT * FROM u"]
              ["SELECT * FROM v"]
              ["SELECT * FROM v"]
              ["INSERT INTO v(c, d, a, e, f, b) VALUES (?, ?, ?, ?, ?, ?)"
               6
               12104218
               8047261
               -1056524485
               -2142060654
               87129]
              ["SELECT * FROM u"]
              ["INSERT INTO v(c, a, e, b, d, f) VALUES (?, ?, ?, ?, ?, ?)"
               384963227
               18
               -153
               623602
               -2
               0]]
             (mapv sql (:statements case)))))))

