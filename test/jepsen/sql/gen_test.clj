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
      (is (= (mapv ->Statement
                   [["CREATE TABLE t (b int PRIMARY KEY, a int, c int, d int)"]
                    ["CREATE TABLE v (a int, c int PRIMARY KEY, b int, d int)"]
                    ["CREATE TABLE u (d int)"]])
             (setup case)))
      (is (= (mapv ->Statement [["DROP TABLE t"] ["DROP TABLE v"] ["DROP TABLE u"]])
             (teardown case)))
      (is (= [
              ["SELECT * FROM v"]
           ["SELECT * FROM v"]
           ["UPDATE v SET b = ?, d = ?, a = ?, c = ? WHERE (? = a)"
            7
            -3805594
            9
            -25
            726]
           ["SELECT * FROM v WHERE (b = c)"]
           ["INSERT INTO v (a, c, b, d) VALUES ?, ?, ?, ?"
            1
            -12
            176
            -299907300]
           ["UPDATE t SET d = ? WHERE (? = c)" 129876556 21229]
           ["SELECT * FROM v"]
           ["SELECT * FROM u WHERE (d = ?)" 36092]
           ["INSERT INTO u (d) VALUES ?" -1]
           ["SELECT * FROM u WHERE (? = ?)" -36273 57]
           ["UPDATE v SET a = ?, b = ?, d = ?, c = ? WHERE (c = c)"
            -162
            -3
            -107191
            1072218635]
           ["INSERT INTO v (a, c, b, d) VALUES ?, ?, ?, ?"
            -40029
            -300
            -15583
            3]
           ["INSERT INTO v (c, d, b, a) VALUES ?, ?, ?, ?"
            384963227
            18
            -153
            -2]
              ]
             (mapv sql (:statements case)))))))

