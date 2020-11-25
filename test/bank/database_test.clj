(ns bank.database-test
  (:require
    [clojure.test :refer :all]
    [bank.database :refer :all]
    [next.jdbc :as sql]))

(deftest db-empty-table-test
  (testing "after drop-tables, the database should be empty"
    (drop-tables default-ds)
    (create-tables default-ds)
    (let [result (sql/execute! default-ds ["select * from account"])]
      (is (= [] result)))))

(deftest db-create-account-test
  (testing "create an account and see if it's there"
    (drop-tables default-ds)
    (create-tables default-ds)
    (create-account default-ds "Mr. White")
    (let [result (sql/execute! default-ds ["select * from account"])]
      ; today I learned something new: map namespace syntax. The following two are equivalent:
      (is (= [{:account/id 1, :account/name "Mr. White", :account/balance 0.0}] result))
      (is (= [#:account{:id 1, :name "Mr. White", :balance 0.0}])))))

(deftest db-get-account-test
  (testing "create some accounts and query them"
    (drop-tables default-ds)
    (create-tables default-ds)
    (create-account default-ds "Mr. White")
    (create-account default-ds "Mr. Orange")
    (create-account default-ds "Mr. Black")
    (is (= [#:account{:id 1, :name "Mr. White", :balance 0.0}] (get-account default-ds 1)))
    (is (= [#:account{:id 2, :name "Mr. Orange", :balance 0.0}] (get-account default-ds 2)))
    (is (= [#:account{:id 3, :name "Mr. Black", :balance 0.0}] (get-account default-ds 3)))))
