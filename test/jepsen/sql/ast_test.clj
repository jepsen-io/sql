(ns jepsen.sql.ast-test
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
