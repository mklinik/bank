(ns bank.database-test
  (:require
    [clojure.test :refer :all]
    [bank.database :as db]))

(deftest db-create-account-test
  (testing "create an account"
    (is (= 1 2))))
