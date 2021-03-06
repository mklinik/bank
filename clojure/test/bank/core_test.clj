(ns bank.core-test
  (:require
    [clojure.test :refer :all]
    [clojure.java.shell :refer :all]
    [cheshire.core :as json]
    [bank.core :as bank]
    [bank.database :as db]
    [bank.util :refer :all]
    ))

; A fixture that starts the bank app in jetty, runs the test f, then stops the server
; https://stackoverflow.com/questions/24480996/how-to-do-integration-testing-for-clojure-ring-selenium-leiningen
; TODO: We also want to clear the database, so each test runs in a fresh database
; TODO: Find a way to test persistence between server restarts
(defn with-ring [f]
  (let [server (ring.adapter.jetty/run-jetty bank/app {:port 3000 :join? false})]
    (try
      (f)
      (finally
        (.stop server)))))

; start server once for all tests
(use-fixtures :once with-ring)

; Performs a POST request with the given input map as json body, and returns the
; result json as decoded map
(defn curl-post [url input]
  (json/decode (:out (sh "curl" "-q" url
    "-H" "Content-Type: application/json"
    "-d" (json/encode input)))))

; Performs a GET request to the given url and returns the response json as
; decoded map
(defn curl-get [url]
  (json/decode (:out (sh "curl" "-q" url))))

(deftest create-account-test
  (testing "create an account and check the returned response"
    (db/reset db/default-ds)
    (is (= {"account-number" 1
            "name" "Mr. Orange"
            "balance" 0.0}
           (curl-post "http://localhost:3000/account" {"name" "Mr. Orange"})))))


(deftest create-two-accounts-test
  (testing "create two accounts, they should get consecutive numbers"
    (db/reset db/default-ds)
    (is (= {"account-number" 1
            "name" "Mr. Orange"
            "balance" 0.0}
           (curl-post "http://localhost:3000/account" {"name" "Mr. Orange"})))
    (is (= {"account-number" 2
            "name" "Mr. White"
            "balance" 0.0}
           (curl-post "http://localhost:3000/account" {"name" "Mr. White"})))))


(deftest create-and-then-retrieve-test
  (testing "create an account and then retieve it"
    (db/reset db/default-ds)
    (is (= {"account-number" 1
            "name" "Mr. Orange"
            "balance" 0.0}
           (curl-post "http://localhost:3000/account" {"name" "Mr. Orange"})))
    (is (= {"account-number" 1
            "name" "Mr. Orange"
            "balance" 0.0}
           (curl-get  "http://localhost:3000/account/1")))))


(deftest create-and-then-retrieve-multiple-test
  (testing "create multiple accounts and then retieve them"
    (db/reset db/default-ds)
    (curl-post "http://localhost:3000/account" {"name" "Mr. Orange"})
    (curl-post "http://localhost:3000/account" {"name" "Mr. Black"})
    (curl-post "http://localhost:3000/account" {"name" "Mr. White"})
    (curl-post "http://localhost:3000/account" {"name" "Mr. Pink"})
    (is (= {"account-number" 1
            "name" "Mr. Orange"
            "balance" 0.0}
           (curl-get  "http://localhost:3000/account/1")))
    (is (= {"account-number" 2
            "name" "Mr. Black"
            "balance" 0.0}
           (curl-get  "http://localhost:3000/account/2")))
    (is (= {"account-number" 3
            "name" "Mr. White"
            "balance" 0.0}
           (curl-get  "http://localhost:3000/account/3")))
    (is (= {"account-number" 4
            "name" "Mr. Pink"
            "balance" 0.0}
           (curl-get  "http://localhost:3000/account/4")))
))


(deftest deposit-test
  (testing "deposit money to an account"
    (db/reset db/default-ds)
    (curl-post "http://localhost:3000/account" {"name" "Mr. Orange"})
    (is (= {"account-number" 1
            "name" "Mr. Orange"
            "balance" 0.0}
           (curl-get  "http://localhost:3000/account/1")))
    (is (= {"account-number" 1
            "name" "Mr. Orange"
            "balance" 100.0 }
           (curl-post "http://localhost:3000/account/1/deposit" {"amount" 100})))
))


(deftest deposit-negative-test
  (testing "try to deposit a negative amount"
    (db/reset db/default-ds)
    (curl-post "http://localhost:3000/account" {"name" "Mr. Orange"})
    (is (= {"account-number" 1
            "name" "Mr. Orange"
            "balance" 0.0}
           (curl-get  "http://localhost:3000/account/1")))
    (is (= {}
           (curl-post "http://localhost:3000/account/1/deposit" {"amount" -100})))
    (is (= {"account-number" 1
            "name" "Mr. Orange"
            "balance" 0.0}
           (curl-get  "http://localhost:3000/account/1")))
))


(deftest verify-deposit-parameters-test
  (testing "verify the parameters of a request to the deposit handler"
    (is (= {:amount 100 :id 15} (bank/verify-deposit-parameters {:body {"amount" 100} :route-params {:id "15"}})))
    (is (= nil (bank/verify-deposit-parameters {:body {"amount" -100} :route-params {:id "15"}})))
    (is (= nil (bank/verify-deposit-parameters {:body {"amount" 100} :route-params {:id "abc"}})))
    (is (= nil (bank/verify-deposit-parameters {:body {"amount" 100} :route-params {:id "1a5"}})))
))


(deftest withdraw-test
  (testing "withdraw money from an account"
    (db/reset db/default-ds)
    (curl-post "http://localhost:3000/account" {"name" "Mr. Orange"})
    (is (= {"account-number" 1
            "name" "Mr. Orange"
            "balance" 100.0 }
           (curl-post "http://localhost:3000/account/1/deposit" {"amount" 100})))
    (is (= {"account-number" 1
            "name" "Mr. Orange"
            "balance" 75.0 }
           (curl-post "http://localhost:3000/account/1/withdraw" {"amount" 25})))
))


(deftest withdraw-too-much-test
  (testing "withdraw money too much money from an account should be a noop"
    (db/reset db/default-ds)
    (curl-post "http://localhost:3000/account" {"name" "Mr. Orange"})
    (is (= {"account-number" 1
            "name" "Mr. Orange"
            "balance" 100.0 }
           (curl-post "http://localhost:3000/account/1/deposit" {"amount" 100})))
    (is (= {"account-number" 1
            "name" "Mr. Orange"
            "balance" 100.0 }
           (curl-post "http://localhost:3000/account/1/withdraw" {"amount" 2000})))
))


(deftest withdraw-negative-test
  (testing "withdraw a negative amount should fail; actually it responds with 400, which we should test for"
    (db/reset db/default-ds)
    (curl-post "http://localhost:3000/account" {"name" "Mr. Orange"})
    (is (= {"account-number" 1
            "name" "Mr. Orange"
            "balance" 100.0 }
           (curl-post "http://localhost:3000/account/1/deposit" {"amount" 100})))
    (is (= {}
           (curl-post "http://localhost:3000/account/1/withdraw" {"amount" -2})))
))


(deftest withdraw-from-non-existing-account-test
  (testing "withdraw from a nonexistent account should fail"
    (db/reset db/default-ds)
    (curl-post "http://localhost:3000/account" {"name" "Mr. Orange"})
    (is (= {"account-number" 1
            "name" "Mr. Orange"
            "balance" 100.0 }
           (curl-post "http://localhost:3000/account/1/deposit" {"amount" 100})))
    (is (= {}
           (curl-post "http://localhost:3000/account/47/withdraw" {"amount" 20})))
    (is (= {"account-number" 1
            "name" "Mr. Orange"
            "balance" 100.0}
           (curl-get  "http://localhost:3000/account/1")))
))


(deftest withdraw-with-invalid-account-number-test
  (testing "withdraw parameter is not a number"
    (db/reset db/default-ds)
    (curl-post "http://localhost:3000/account" {"name" "Mr. Orange"})
    (is (= {"account-number" 1
            "name" "Mr. Orange"
            "balance" 100.0 }
           (curl-post "http://localhost:3000/account/1/deposit" {"amount" 100})))
    (is (= {}
           (curl-post "http://localhost:3000/account/abcd/withdraw" {"amount" 20})))
    (is (= {"account-number" 1
            "name" "Mr. Orange"
            "balance" 100.0}
           (curl-get  "http://localhost:3000/account/1")))
))


(deftest transfer-test
  (testing "just transfer some money"
    (db/reset db/default-ds)
    (curl-post "http://localhost:3000/account" {"name" "Mr. Orange"})
    (curl-post "http://localhost:3000/account" {"name" "Mr. Black"})
    (curl-post "http://localhost:3000/account/1/deposit" {"amount" 100})
    (is (= {"account-number" 1
            "name" "Mr. Orange"
            "balance" 100.0}
           (curl-get  "http://localhost:3000/account/1")))

    (is (= {"account-number" 1
            "name" "Mr. Orange"
            "balance" 80.0}
           (curl-post "http://localhost:3000/account/1/send" {"amount" 20, "account-number" 2})))

    (is (= {"account-number" 1
            "name" "Mr. Orange"
            "balance" 80.0}
           (curl-get  "http://localhost:3000/account/1")))
    (is (= {"account-number" 2
            "name" "Mr. Black"
            "balance" 20.0}
           (curl-get  "http://localhost:3000/account/2")))
))


(deftest transfer-invalid-parameters-test
  (testing "transfer negative amount"
    (db/reset db/default-ds)
    (curl-post "http://localhost:3000/account" {"name" "Mr. Orange"})
    (curl-post "http://localhost:3000/account" {"name" "Mr. Black"})
    (curl-post "http://localhost:3000/account/1/deposit" {"amount" 100})

    (is (= {}
           (curl-post "http://localhost:3000/account/1/send" {"amount" -20, "account-number" 2})))

    (is (= {"account-number" 1
            "name" "Mr. Orange"
            "balance" 100.0}
           (curl-get  "http://localhost:3000/account/1")))
    (is (= {"account-number" 2
            "name" "Mr. Black"
            "balance" 0.0}
           (curl-get  "http://localhost:3000/account/2"))))

  (testing "transfer from nonexistent account"
    (db/reset db/default-ds)
    (curl-post "http://localhost:3000/account" {"name" "Mr. Orange"})
    (curl-post "http://localhost:3000/account" {"name" "Mr. Black"})
    (curl-post "http://localhost:3000/account/1/deposit" {"amount" 100})

    (is (= {}
           (curl-post "http://localhost:3000/account/101/send" {"amount" 20, "account-number" 2})))

    (is (= {"account-number" 1
            "name" "Mr. Orange"
            "balance" 100.0}
           (curl-get  "http://localhost:3000/account/1")))
    (is (= {"account-number" 2
            "name" "Mr. Black"
            "balance" 0.0}
           (curl-get  "http://localhost:3000/account/2"))))

  (testing "transfer to nonexistent account"
    (db/reset db/default-ds)
    (curl-post "http://localhost:3000/account" {"name" "Mr. Orange"})
    (curl-post "http://localhost:3000/account" {"name" "Mr. Black"})
    (curl-post "http://localhost:3000/account/1/deposit" {"amount" 100})

    (is (= {}
           (curl-post "http://localhost:3000/account/1/send" {"amount" 20, "account-number" 13})))

    (is (= {"account-number" 1
            "name" "Mr. Orange"
            "balance" 100.0}
           (curl-get  "http://localhost:3000/account/1")))
    (is (= {"account-number" 2
            "name" "Mr. Black"
            "balance" 0.0}
           (curl-get  "http://localhost:3000/account/2"))))

  (testing "transfer to self"
    (db/reset db/default-ds)
    (curl-post "http://localhost:3000/account" {"name" "Mr. Orange"})
    (curl-post "http://localhost:3000/account/1/deposit" {"amount" 100})

    (is (= {}
           (curl-post "http://localhost:3000/account/1/send" {"amount" 20, "account-number" 1})))

    (is (= {"account-number" 1
            "name" "Mr. Orange"
            "balance" 100.0}
           (curl-get  "http://localhost:3000/account/1"))))
)



(deftest get-audit-log-test
  (testing "the audit log as it is in the spec"
    (db/reset db/default-ds)
    (curl-post "http://localhost:3000/account" {"name" "Mr. Orange"}) ; account 1
    (curl-post "http://localhost:3000/account" {"name" "Mr. Black"})  ; account 2 (800 in the spec)
    (curl-post "http://localhost:3000/account" {"name" "Mr. Pink"})   ; account 3 (900 in the spec)
    (curl-post "http://localhost:3000/account/1/deposit" {"amount" 100})
    (curl-post "http://localhost:3000/account/2/deposit" {"amount" 100})
    (curl-post "http://localhost:3000/account/1/send" {"amount" 5, "account-number" 3})
    (curl-post "http://localhost:3000/account/2/send" {"amount" 10, "account-number" 1})
    (curl-post "http://localhost:3000/account/1/withdraw" {"amount" 20})

    (is (= [
              {
                  "sequence" 3,
                  "debit" 20,
                  "description" "withdraw"
              },
              {
                  "sequence" 2,
                  "credit" 10,
                  "description" "receive from #2"
              },
              {
                  "sequence" 1,
                  "debit" 5,
                  "description" "send to #3"
              },
              {
                  "sequence" 0,
                  "credit" 100,
                  "description" "deposit"
              }
           ]
           (curl-get "http://localhost:3000/account/1/audit")))
))
