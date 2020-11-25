(ns bank.database-test
  (:require
    [clojure.test :refer :all]
    [bank.database :refer :all]
    [next.jdbc :as jdbc]))

(def test-db {:dbtype "postgresql" :host "localhost" :dbname "bank-test" :user "mkl" :password "w00t"})
(def test-ds (jdbc/get-datasource test-db))

(deftest db-empty-table-test
  (testing "after drop-tables, the database should be empty"
    (drop-tables test-ds)
    (create-tables test-ds)
    (is (= [] (jdbc/execute! test-ds ["select * from account"])))))

(deftest db-create-account-test
  (testing "create an account and see if it's there"
    (drop-tables test-ds)
    (create-tables test-ds)
    (create-account test-ds "Mr. White")
    (let [result (jdbc/execute! test-ds ["select * from account"])]
      ; today I learned something new: map namespace syntax. The following two are equivalent:
      (is (= [{:account/id 1, :account/name "Mr. White", :account/balance 0.0}] result))
      (is (= [#:account{:id 1, :name "Mr. White", :balance 0.0}])))))

(deftest db-get-account-test
  (testing "create some accounts and query them"
    (drop-tables test-ds)
    (create-tables test-ds)
    (create-account test-ds "Mr. White")
    (create-account test-ds "Mr. Orange")
    (create-account test-ds "Mr. Black")
    (is (= #:account{:id 1, :name "Mr. White", :balance 0.0} (get-account test-ds 1)))
    (is (= #:account{:id 2, :name "Mr. Orange", :balance 0.0} (get-account test-ds 2)))
    (is (= #:account{:id 3, :name "Mr. Black", :balance 0.0} (get-account test-ds 3)))))


(deftest db-deposit-test
  (testing "deposit some money to an account"
    (drop-tables test-ds)
    (create-tables test-ds)
    (create-account test-ds "Mr. White")
    (is (= #:account{:id 1, :name "Mr. White", :balance 0.0} (get-account test-ds 1)))
    (is (= #:account{:id 1, :name "Mr. White", :balance 20.0} (deposit test-ds 1 20)))
    (is (= #:account{:id 1, :name "Mr. White", :balance 20.0} (get-account test-ds 1)))
    (is (= #:account{:id 1, :name "Mr. White", :balance 20.5} (deposit test-ds 1 0.5)))
    (is (= #:account{:id 1, :name "Mr. White", :balance 20.5} (get-account test-ds 1)))
    (is (= #:account{:id 1, :name "Mr. White", :balance 20.55} (deposit test-ds 1 0.05)))
    (is (= #:account{:id 1, :name "Mr. White", :balance 20.55} (get-account test-ds 1)))
))


(deftest db-deposit-only-one-test
  (testing "deposit some money to an account, make sure that only this account changes"
    (drop-tables test-ds)
    (create-tables test-ds)
    (create-account test-ds "Mr. White")
    (create-account test-ds "Mr. Orange")
    (is (= #:account{:id 1, :name "Mr. White", :balance 0.0} (get-account test-ds 1)))
    (is (= #:account{:id 2, :name "Mr. Orange", :balance 0.0} (get-account test-ds 2)))
    (deposit test-ds 2 20)
    (is (= #:account{:id 1, :name "Mr. White", :balance 0.0} (get-account test-ds 1)))
    (is (= #:account{:id 2, :name "Mr. Orange", :balance 20.0} (get-account test-ds 2)))
))


(deftest db-deposit-nonexistent-test
  (testing "deposit some money to a nonexistent account"
    (drop-tables test-ds)
    (create-tables test-ds)
    (create-account test-ds "Mr. White")
    (is (= #:account{:id 1, :name "Mr. White", :balance 0.0} (get-account test-ds 1)))
    (is (= nil (deposit test-ds 100 20))) ; account 100 doesn't exist
    (is (= #:account{:id 1, :name "Mr. White", :balance 0.0} (get-account test-ds 1)))
))
