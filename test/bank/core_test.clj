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

; restart the server for each test
(use-fixtures :each with-ring)

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
