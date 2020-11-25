(ns bank.database-test
  (:require
    [clojure.test :refer :all]
    [bank.database :as db]
    [next.jdbc :as sql]))

(deftest db-query-cities-test
  (testing "query the test database"
    (let
      [dbmeta {:dbtype "postgresql" :dbname "test" :user "mkl" :password "w00t"}
       ds (sql/get-datasource dbmeta)
       result (sql/execute! ds ["select * from cities"])]
      (is (= () result)))))
