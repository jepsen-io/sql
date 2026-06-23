(ns jepsen.sql.ast-test
  (:refer-clojure :exclude [eval])
  (:require [clojure [test :refer :all]]
            [jepsen.sql.ast :refer :all]))

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

(deftest equals-test
  (is (false? (eval (->Equals (->Literal 1) (->Literal 2)) {:x 1 :y 2})))
  (is (true?  (eval (->Equals (->Literal 2) (->Literal 2)) {:x 1 :y 2})))
  (is (false? (eval (->Equals (->Literal 1) (column-name "y")) {:x 1 :y 2})))
  (is (true?  (eval (->Equals (->Literal 1) (column-name "x")) {:x 1 :y 2})))
  )

(deftest not-test
  (is (false? (eval (->Not (->Literal true)) nil)))
  (is (true?  (eval (->Not (->Literal false)) nil))))

(deftest and-test
  (is (false? (eval (->And [(->Literal true)
                            (column-name "x")
                            (column-name "y")])
                    {:x false :y true})))
  (is (true? (eval (->And [(->Literal true)
                           (column-name "x")
                           (column-name "y")])
                   {:x true :y true}))))

(deftest or-test
  (is (false? (eval (->Or [(->Literal false)
                            (column-name "x")
                            (column-name "y")])
                    {:x false :y false})))
  (is (true? (eval (->Or [(->Literal false)
                           (column-name "x")
                           (column-name "y")])
                   {:x true :y false}))))
