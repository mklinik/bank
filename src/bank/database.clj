(ns bank.database
  (:require
    [clojure.set :refer [rename-keys, map-invert]]
    [next.jdbc :as jdbc]
    [next.jdbc.result-set :refer [as-unqualified-kebab-maps]]
    [next.jdbc.sql :as sql]))

(def default-db {:dbtype "postgresql" :host "localhost" :dbname "bank" :user "mkl" :password "w00t"})
(def default-ds (jdbc/get-datasource default-db))

(defn drop-tables [ds]
  (jdbc/execute-one! ds ["drop table if exists account"]))

(defn create-tables [ds]
  (jdbc/execute-one! ds [(str
    "create table if not exists account"
    "( account_number serial not null"
    ", name text not null"
    ", balance money"
    ", primary key (account_number))")]))

(defn reset [ds] (do
  (drop-tables ds)
  (create-tables ds)))

; TODO: take care of sql injection: how?
; Create an account with the given name. Returns the created row as map. Note:
; this returns the complete row only for postgresql. Other databases may return
; only the keys. See next.jdbc.sql documentation.
(defn create-account [ds name]
  (sql/insert! ds :account {:name name :balance 0} {:builder-fn as-unqualified-kebab-maps}))

; Returns nil if an account with the given id doesn't exist.
(defn get-account [ds id]
  (sql/get-by-id ds :account id "account_number" {:builder-fn as-unqualified-kebab-maps}))

; Deposits the given amount to the account and returns the updated account.
; Does not perform argument verification. Only pass positive amounts!
(defn deposit [ds id amount]
  ; We're using a transaction to make sure that the read after the update reads
  ; exactly what was updated. No other transaction should come in between.
  ; TODO: the default transaction isolation level of postgres is Read
  ; Committed. Is this enough for our purpose?
  (jdbc/with-transaction [conn ds]
    ; important! use conn for all executions, not ds
    (jdbc/execute-one! conn [(str
      "update account"
      " set balance = balance + ?::numeric::money"
      " where account_number = ?") amount id])
    (get-account conn id)))


; The where statement assures that balance can't fall below zero, and that the
; amount is positive.
(defn withdraw [ds id amount]
  (jdbc/with-transaction [conn ds]
    (jdbc/execute-one! conn [(str
      "update account"
      " set balance = balance - ?::numeric::money"
      " where account_number = ?"
      " and balance >= ?::numeric::money"
      " and ? > 0") amount id amount amount])
    (get-account conn id)))
