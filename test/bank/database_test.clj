(ns bank.database-test
  (:require
    [clojure.test :refer :all]
    [bank.database :refer :all]
    [next.jdbc :as jdbc]
    [next.jdbc.result-set :refer :all]
    [bank.util :refer :all]))

(def test-db {:dbtype "postgresql" :host "localhost" :dbname "bank-test" :user "mkl" :password "w00t"})
(def test-ds (jdbc/get-datasource test-db))

(deftest db-empty-table-test
  (testing "after drop-tables, the database should be empty"
    (drop-tables test-ds)
    (create-tables test-ds)
    (is (= [] (jdbc/execute! test-ds ["select * from account"])))
    (is (= [] (jdbc/execute! test-ds ["select * from audit_log"])))
    (is (= [] (jdbc/execute! test-ds ["select * from next_sequence"])))))

(deftest db-create-account-test
  (testing "create an account and see if it's there"
    (reset test-ds)
    (create-account test-ds "Mr. White")
    (is (= [{:account-number 1, :name "Mr. White", :balance 0.0}]
           (jdbc/execute! test-ds ["select * from account"] {:builder-fn as-unqualified-kebab-maps})))
    (is (= [#:next_sequence{:account_number 1, :next_sequence 0}]
           (jdbc/execute! test-ds ["select * from next_sequence"]))))

  (testing "create two accounts and see if they are there"
    (reset test-ds)
    (create-account test-ds "Mr. White")
    (create-account test-ds "Mr. Orange")
    (is (= [{:account-number 1, :name "Mr. White", :balance 0.0}
            {:account-number 2, :name "Mr. Orange", :balance 0.0}
           ]
           (jdbc/execute! test-ds ["select * from account"] {:builder-fn as-unqualified-kebab-maps})))
    (is (= [#:next_sequence{:account_number 1, :next_sequence 0}
            #:next_sequence{:account_number 2, :next_sequence 0}]
           (jdbc/execute! test-ds ["select * from next_sequence"]))))
)


(deftest db-get-account-test
  (testing "create some accounts and query them"
    (drop-tables test-ds)
    (create-tables test-ds)
    (create-account test-ds "Mr. White")
    (create-account test-ds "Mr. Orange")
    (create-account test-ds "Mr. Black")
    (is (= {:account-number 1, :name "Mr. White", :balance 0.0} (get-account test-ds 1)))
    (is (= {:account-number 2, :name "Mr. Orange", :balance 0.0} (get-account test-ds 2)))
    (is (= {:account-number 3, :name "Mr. Black", :balance 0.0} (get-account test-ds 3)))))


(deftest db-deposit-test
  (testing "deposit some money to an account"
    (reset test-ds)
    (create-account test-ds "Mr. White")
    (is (= {:account-number 1, :name "Mr. White", :balance 0.0} (get-account test-ds 1)))
    (is (= {:account-number 1, :name "Mr. White", :balance 20.0} (deposit test-ds 1 20)))
    (is (= {:account-number 1, :name "Mr. White", :balance 20.0} (get-account test-ds 1)))
    (is (= {:account-number 1, :name "Mr. White", :balance 20.5} (deposit test-ds 1 0.5)))
    (is (= {:account-number 1, :name "Mr. White", :balance 20.5} (get-account test-ds 1)))
    (is (= {:account-number 1, :name "Mr. White", :balance 20.55} (deposit test-ds 1 0.05)))
    (is (= {:account-number 1, :name "Mr. White", :balance 20.55} (get-account test-ds 1))))

  (testing "deposit some money to an account and check the audit log"
    (reset test-ds)
    (create-account test-ds "Mr. White")
    (is (= {:account-number 1, :name "Mr. White", :balance 20.0} (deposit test-ds 1 20)))
    (is (= [#:audit_log
            {:sequence 0
             :account_number 1
             :debit nil
             :credit 20
             :description "deposit"
            }]
           (jdbc/execute! test-ds ["select * from audit_log"]))))
)


(deftest db-deposit-only-one-test
  (testing "deposit some money to an account, make sure that only this account changes"
    (drop-tables test-ds)
    (create-tables test-ds)
    (create-account test-ds "Mr. White")
    (create-account test-ds "Mr. Orange")
    (is (= {:account-number 1, :name "Mr. White", :balance 0.0} (get-account test-ds 1)))
    (is (= {:account-number 2, :name "Mr. Orange", :balance 0.0} (get-account test-ds 2)))
    (deposit test-ds 2 20)
    (is (= {:account-number 1, :name "Mr. White", :balance 0.0} (get-account test-ds 1)))
    (is (= {:account-number 2, :name "Mr. Orange", :balance 20.0} (get-account test-ds 2)))
))


(deftest db-deposit-negative-test
  (testing "deposit a negative amount is a noop"
    (drop-tables test-ds)
    (create-tables test-ds)
    (create-account test-ds "Mr. White")
    (is (= {:account-number 1, :name "Mr. White", :balance 0.0} (get-account test-ds 1)))
    (deposit test-ds 1 -20)
    (is (= {:account-number 1, :name "Mr. White", :balance 0.0} (get-account test-ds 1)))
))


(deftest db-deposit-nonexistent-test
  (testing "deposit some money to a nonexistent account"
    (drop-tables test-ds)
    (create-tables test-ds)
    (create-account test-ds "Mr. White")
    (is (= {:account-number 1, :name "Mr. White", :balance 0.0} (get-account test-ds 1)))
    (is (= nil (deposit test-ds 100 20))) ; account 100 doesn't exist
    (is (= {:account-number 1, :name "Mr. White", :balance 0.0} (get-account test-ds 1)))
))


(deftest builder-fn-test
  (testing "modify column names with a jdbc builder-fn"
    (drop-tables test-ds)
    (create-tables test-ds)
    (create-account test-ds "Mr. White")
    (is (= {:account-number 1, :name "Mr. White", :balance 0.0} (jdbc/execute-one! test-ds ["select * from account where account_number = ?" 1] {:builder-fn as-unqualified-kebab-maps})))
))


(deftest withdraw-test
  (testing "withdraw money from an account"
    (reset test-ds)
    (create-account test-ds "Mr. White")
    (deposit test-ds 1 20)
    (is (= {:account-number 1, :name "Mr. White", :balance 15.0} (withdraw test-ds 1 5)))
    (is (= {:account-number 1, :name "Mr. White", :balance 15.0} (get-account test-ds 1)))
    (is (= {:account-number 1, :name "Mr. White", :balance 5.0} (withdraw test-ds 1 10))))
  (testing "deposit then withdraw money from an account and check the audit log"
    (reset test-ds)
    (create-account test-ds "Mr. White")
    (deposit test-ds 1 20)
    (withdraw test-ds 1 5)
    (is (= [#:audit_log{:sequence 0 :account_number 1 :debit nil :credit  20 :description "deposit"}
           ,#:audit_log{:sequence 1 :account_number 1 :debit   5 :credit nil :description "withdraw"}
           ]
           (jdbc/execute! test-ds ["select * from audit_log"]))))
)


(deftest withdraw-too-much-test
  (testing "withdraw more money from an account than whats on it"
    (drop-tables test-ds)
    (create-tables test-ds)
    (create-account test-ds "Mr. White")
    (deposit test-ds 1 20)
    (is (= {:account-number 1, :name "Mr. White", :balance 20.0} (withdraw test-ds 1 100)))
))


(deftest withdraw-from-non-existing-account-test
  (testing "withdraw money from a non-existing account should return nil"
    (drop-tables test-ds)
    (create-tables test-ds)
    (create-account test-ds "Mr. White")
    (deposit test-ds 1 20)
    (is (= nil (withdraw test-ds 47 100)))
    (is (= {:account-number 1, :name "Mr. White", :balance 20.0} (withdraw test-ds 1 100)))
))


(deftest withdraw-negative-amount-test
  (testing "withdrawing a negative amount account should be a noop"
    (drop-tables test-ds)
    (create-tables test-ds)
    (create-account test-ds "Mr. White")
    (deposit test-ds 1 20)
    (is (= {:account-number 1, :name "Mr. White", :balance 20.0} (withdraw test-ds 1 -100)))
))


(deftest transfer-param-verification-test
  (testing "transfer money from a nonexistent account to an existing account"
    (drop-tables test-ds)
    (create-tables test-ds)
    (create-account test-ds "Mr. White")
    (create-account test-ds "Mr. Pink")
    (deposit test-ds 1 20)
    (is (= nil (transfer test-ds 47 1 100))))
  (testing "transfer money to a nonexistent account"
    (drop-tables test-ds)
    (create-tables test-ds)
    (create-account test-ds "Mr. White")
    (create-account test-ds "Mr. Pink")
    (deposit test-ds 1 20)
    (is (= nil (transfer test-ds 1 47 100))))
  (testing "transfer between nonexistent accounts"
    (drop-tables test-ds)
    (create-tables test-ds)
    (create-account test-ds "Mr. White")
    (create-account test-ds "Mr. Pink")
    (deposit test-ds 1 20)
    (is (= nil (transfer test-ds 1000 47 100))))
  (testing "transfer to self"
    (drop-tables test-ds)
    (create-tables test-ds)
    (create-account test-ds "Mr. White")
    (create-account test-ds "Mr. Pink")
    (deposit test-ds 1 20)
    (is (= nil (transfer test-ds 1 1 5))))
  (testing "insufficient funds on sender account"
    (drop-tables test-ds)
    (create-tables test-ds)
    (create-account test-ds "Mr. White")
    (create-account test-ds "Mr. Pink")
    (deposit test-ds 1 20)
    (is (= nil (transfer test-ds 1 2 5000))))
)


(deftest transfer-test
  (testing "transfer some money"
    (drop-tables test-ds)
    (create-tables test-ds)
    (create-account test-ds "Mr. White")
    (create-account test-ds "Mr. Pink")
    (deposit test-ds 1 20)
    (is (= {:account-number 1, :name "Mr. White", :balance 20.0} (get-account test-ds 1)))
    (is (= {:account-number 2, :name "Mr. Pink",  :balance  0.0} (get-account test-ds 2)))

    (is (= {:account-number 1, :name "Mr. White", :balance 15.0} (transfer test-ds 1 2 5)))

    (is (= {:account-number 1, :name "Mr. White", :balance 15.0} (get-account test-ds 1)))
    (is (= {:account-number 2, :name "Mr. Pink",  :balance  5.0} (get-account test-ds 2)))
))


(deftest transfer-auditlog-test
  (testing "transfer some money and check the audit logs"
    (reset test-ds)
    (create-account test-ds "Mr. White")
    (create-account test-ds "Mr. Pink")
    (deposit test-ds 1 20)
    (transfer test-ds 1 2 5)
    (is (= [#:audit_log{:sequence 0 :account_number 1 :debit nil :credit  20 :description "deposit"}
           ,#:audit_log{:sequence 1 :account_number 1 :debit   5 :credit nil :description "send to #2"}
           ]
           (jdbc/execute! test-ds ["select * from audit_log where account_number = 1"])))
    (is (= [#:audit_log{:sequence 0 :account_number 2 :debit nil :credit   5 :description "receive from #1"}
           ]
           (jdbc/execute! test-ds ["select * from audit_log where account_number = 2"])))
))


(deftest get-audit-log-test
  (testing "transfer some money and check the audit logs obtained from get-audit-log"
    (reset test-ds)
    (create-account test-ds "Mr. White")
    (create-account test-ds "Mr. Pink")
    (deposit test-ds 1 20)
    (transfer test-ds 1 2 5)
    (withdraw test-ds 2 5)
    (is (= [{:sequence 1, :debit   5, :credit nil, :description "send to #2"}
            {:sequence 0, :debit nil, :credit  20, :description "deposit"}]
           (get-audit-log test-ds 1)))
    (is (= [{:sequence 1, :debit   5, :credit nil, :description "withdraw"}
            {:sequence 0, :debit nil, :credit   5, :description "receive from #1"}]
           (get-audit-log test-ds 2)))
))
