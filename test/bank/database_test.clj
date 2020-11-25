(ns bank.database-test
  (:require
    [clojure.test :refer :all]
    [bank.database :refer :all]
    [next.jdbc :as sql]))

(deftest db-empty-table-test
  (testing "after drop-tables, the database should be empty"
    (let
      [result (do
        (drop-tables default-ds)
        (create-tables default-ds)
        (sql/execute! default-ds ["select * from account"]))]
      (is (= [] result)))))

(deftest db-create-account-test
  (testing "create an account and see if it's there"
    (let
      [result (do
        (drop-tables default-ds)
        (create-tables default-ds)
        (create-account default-ds "Mr. White")
        (sql/execute! default-ds ["select * from account"]))]
      ; today I learned something new: map namespace syntax. The following two are equivalent:
      (is (= [{:account/id 1, :account/name "Mr. White", :account/balance 0.0}] result))
      (is (= [#:account{:id 1, :name "Mr. White", :balance 0.0}])))))
