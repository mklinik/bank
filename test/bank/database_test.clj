(ns bank.database-test
  (:require
    [clojure.test :refer :all]
    [bank.database :refer :all]
    [next.jdbc :as sql]))

(deftest db-empty-table
  (testing "after drop-tables, the database should be empty"
    (let
      [result (do
        (drop-tables default-ds)
        (create-tables default-ds)
        (sql/execute! default-ds ["select * from account"]))]
      (is (= [] result)))))
