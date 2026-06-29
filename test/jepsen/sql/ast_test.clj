(ns jepsen.sql.ast-test
  (:refer-clojure :exclude [eval update])
  (:require [clojure [pprint :refer [pprint]]
                     [test :refer :all]]
            [clojure.test.check.generators :as g]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [jepsen.sql [ast :refer :all]
                        [gen :as gen]]))

(deftest unique-tables-test
  (is (= (->Case
           (->Schema
             [(->Table "meow_2" [])]
             {})
           [(->Insert (->TableName "meow_2")
                      :cols :vals)])
         (unique-tables
           (->Case
             (->Schema
               [(->Table "meow" [])]
               {})
             [(->Insert (->TableName "meow")
                        :cols :vals)])
           2))))

(deftest column-sql-test
  (is (= ["b INTEGER PRIMARY KEY"]
         (sql (column "b" integer-type {:primary-key? true})))))

(deftest table-setup-test
  (is (= [["CREATE TABLE foo (a TEXT, b INTEGER, c BOOLEAN)"]]
         (->> (table "foo" [(column "a" text-type)
                              (column "b" integer-type)
                              (column "c" boolean-type)])
              setup
              (mapv sql)))))

(deftest equals-test
  (is (false? (eval (equals (->Literal 1) (->Literal 2)) {:x 1 :y 2})))
  (is (true?  (eval (equals (->Literal 2) (->Literal 2)) {:x 1 :y 2})))
  (is (false? (eval (equals (->Literal 1) (column-name "y")) {:x 1 :y 2})))
  (is (true?  (eval (equals (->Literal 1) (column-name "x")) {:x 1 :y 2})))
  (testing "null"
    (is (nil? (eval (equals (->Literal nil) (->Literal nil)) nil)))
    (is (nil? (eval (equals (->Literal 1) (->Literal nil))   nil)))
    (is (nil? (eval (equals (->Literal nil) (->Literal 1))   nil)))))

(deftest not-test
  (is (false? (eval (->Not (->Literal true)) nil)))
  (is (true?  (eval (->Not (->Literal false)) nil)))
  (is (nil?   (eval (->Not (->Literal nil)) nil))))

(deftest and-test
  (is (false? (eval (->And [(->Literal true)
                            (column-name "x")
                            (column-name "y")])
                    {:x false :y true})))
  (is (true? (eval (->And [(->Literal true)
                           (column-name "x")
                           (column-name "y")])
                   {:x true :y true})))
  (testing "null"
    (is (nil? (eval (->and [(literal true) (literal nil)]) nil)))
    (is (nil? (eval (->and [(literal nil) (literal true)]) nil)))
    (is (false? (eval (->and [(literal false) (literal nil)]) nil)))
    (is (false? (eval (->and [(literal nil) (literal false)]) nil)))))

(deftest or-test
  (is (false? (eval (->Or [(->Literal false)
                            (column-name "x")
                            (column-name "y")])
                    {:x false :y false})))
  (is (true? (eval (->Or [(->Literal false)
                           (column-name "x")
                           (column-name "y")])
                   {:x true :y false})))
  (testing "null"
    (is (true?   (eval (->or [(literal true)  (literal nil)])   nil)))
    (is (true?   (eval (->or [(literal nil)   (literal true)])  nil)))
    (is (nil?    (eval (->or [(literal false) (literal nil)])   nil)))
    (is (nil?    (eval (->or [(literal nil)   (literal false)]) nil)))))

(deftest simplify-test
  (checking "booleans simplify equivalently" 1000
            [schema (gen/gen-schema {})
             table  (g/elements (:tables schema))
             expr   (gen/gen-expr-boolean {} schema table 6)
             row    (gen/gen-row {} schema table)]
            (let [simple (simplify expr)]
              #_(when (and (not= simple expr)
                         #_(not= (eval expr row)
                               (eval simple row)))
                (prn)
                (prn :row row)
                (prn (sql expr) '-> (eval expr row))
                (prn (sql simple) '-> (eval simple row)))
              (is (= (eval expr row)
                     (eval simple row))))))
