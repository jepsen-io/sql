(ns jepsen.sql.ast-test
  (:require [clojure [pprint :refer [pprint]]
                     [string :as str]
                     [test :refer :all]]
            [clojure.test.check.generators :as g]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [jepsen.sql [ast :as ast]
                        [gen :as gen]]))

(deftest unique-tables-test
  (is (= (ast/->Case
           (ast/->Schema
             [(ast/->Table "meow_2" [])]
             {})
           [(ast/->Insert (ast/->TableName "meow_2")
                          :cols :vals)])
         (ast/unique-tables
           (ast/->Case
             (ast/->Schema
               [(ast/->Table "meow" [])]
               {})
             [(ast/->Insert (ast/->TableName "meow")
                            :cols :vals)])
           2))))

(deftest column-sql-test
  (is (= ["b INTEGER PRIMARY KEY"]
         (ast/sql (ast/column "b" ast/integer-type {:primary-key? true})))))

(deftest table-setup-test
  (is (= [["CREATE TABLE foo (a TEXT collate unicode, b INTEGER, c BOOLEAN)"]]
         (->> (ast/table "foo" [(ast/column "a" ast/text-type)
                                (ast/column "b" ast/integer-type)
                                (ast/column "c" ast/boolean-type)])
              ast/setup
              (mapv ast/sql)))))

(deftest equals-test
  (is (false? (ast/eval (ast/equals (ast/literal 1) (ast/literal 2)) {:x 1 :y 2})))
  (is (true?  (ast/eval (ast/equals (ast/literal 2) (ast/literal 2)) {:x 1 :y 2})))
  (is (false? (ast/eval (ast/equals (ast/literal 1) (ast/column-name "y")) {:x 1 :y 2})))
  (is (true?  (ast/eval (ast/equals (ast/literal 1) (ast/column-name "x")) {:x 1 :y 2})))
  (testing "null"
    (is (nil? (ast/eval (ast/equals (ast/literal nil) (ast/literal nil)) nil)))
    (is (nil? (ast/eval (ast/equals (ast/literal 1)   (ast/literal nil))   nil)))
    (is (nil? (ast/eval (ast/equals (ast/literal nil) (ast/literal 1))   nil)))))

(deftest not-test
  (is (false? (ast/eval (ast/->Not (ast/literal true)) nil)))
  (is (true?  (ast/eval (ast/->Not (ast/literal false)) nil)))
  (is (nil?   (ast/eval (ast/->Not (ast/literal nil)) nil))))

(deftest and-test
  (is (false? (ast/eval (ast/->And [(ast/literal true)
                                    (ast/column-name "x")
                                    (ast/column-name "y")])
                        {:x false :y true})))
  (is (true? (ast/eval (ast/->And [(ast/literal true)
                                   (ast/column-name "x")
                                   (ast/column-name "y")])
                       {:x true :y true})))
  (testing "null"
    (is (nil? (ast/eval (ast/->and [(ast/literal true) (ast/literal nil)]) nil)))
    (is (nil? (ast/eval (ast/->and [(ast/literal nil) (ast/literal true)]) nil)))
    (is (false? (ast/eval (ast/->and [(ast/literal false) (ast/literal nil)]) nil)))
    (is (false? (ast/eval (ast/->and [(ast/literal nil) (ast/literal false)]) nil)))))

(deftest or-test
  (is (false? (ast/eval (ast/->or [(ast/literal false)
                                   (ast/column-name "x")
                                   (ast/column-name "y")])
                        {:x false :y false})))
  (is (true? (ast/eval (ast/->or [(ast/literal false)
                                  (ast/column-name "x")
                                  (ast/column-name "y")])
                       {:x true :y false})))
  (testing "null"
    (is (true?   (ast/eval (ast/->or [(ast/literal true)  (ast/literal nil)])   nil)))
    (is (true?   (ast/eval (ast/->or [(ast/literal nil)   (ast/literal true)])  nil)))
    (is (nil?    (ast/eval (ast/->or [(ast/literal false) (ast/literal nil)])   nil)))
    (is (nil?    (ast/eval (ast/->or [(ast/literal nil)   (ast/literal false)]) nil)))))

(deftest simplify-test
  (checking "booleans simplify equivalently" 1000
            [schema (gen/gen-schema {})
             table  (g/elements (:tables schema))
             expr   (gen/gen-expr-boolean {} schema table 6)
             row    (gen/gen-row {} schema table)]
            (let [simple (ast/simplify expr)]
              #_(when (and (not= simple expr)
                           #_(not= (eval expr row)
                                   (eval simple row)))
                  (prn)
                  (prn :row row)
                  (prn (ast/sql expr) '-> (ast/eval expr row))
                  (prn (ast/sql simple) '-> (ast/eval simple row)))
              (is (= (ast/eval expr row)
                     (ast/eval simple row))))))

(deftest compare+-test
  (let [strings ["" " " " a" "a"]]
    ; Just for playing around at psql
    #_(do (println "DROP TABLE IF EXISTS t;")
        (println "CREATE TABLE t (s text collate unicode);")
        (println (str "INSERT INTO t VALUES "
                      (str/join ", " (map (fn [s] (str "('" s "')")) strings))
                      ";"))
        (println "SELECT CONCAT('\"', s, '\"') FROM t ORDER BY s ASC;"))
    (is (= [
            ""
            " "
            " a"
            "a"
            ]
           (sort ast/compare+ strings)))))
