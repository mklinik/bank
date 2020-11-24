(ns bank.core-test
  (:require
    [clojure.test :refer :all]
    [clojure.java.shell :refer :all]
    [cheshire.core :as json]
    [bank.core :as bank]))

; A fixture that starts the bank app in jetty, runs the test f, then stops the server
; TODO: We also want to clear the database, so each test runs in a fresh database
; TODO: Find a way to test persistence between server restarts
(defn with-ring [f]
  (let [server (ring.adapter.jetty/run-jetty
                      bank/app
                      {:port 3000 :join? false})]
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
  (is (= {"account-number" "1"
          "name" "Mr. Orange"
          "balance" 0}
         (curl-post "http://localhost:3000/account" {"name" "Mr. Orange"}))))

(deftest create-and-then-retrieve-test
  (is (= {"account-number" "1"
          "name" "Mr. Orange"
          "balance" 0}
         (curl-post "http://localhost:3000/account" {"name" "Mr. Orange"})))
  (is (= {"account-number" "1"
          "name" "Mr. Orange"
          "balance" 0}
         (curl-get  "http://localhost:3000/account/1"))))
