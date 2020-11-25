(ns bank.database
  (:require
    [clojure.set :refer [rename-keys, map-invert]]
    [next.jdbc :as jdbc]
    [next.jdbc.sql :as sql]))

(def default-db {:dbtype "postgresql" :host "localhost" :dbname "bank" :user "mkl" :password "w00t"})
(def default-ds (jdbc/get-datasource default-db))

(defn drop-tables [ds]
  (jdbc/execute-one! ds ["drop table if exists account"]))

(defn create-tables [ds]
  (jdbc/execute-one! ds [(str
    "create table if not exists account"
    "( id serial not null"
    ", name text not null"
    ", balance money"
    ", primary key (id))")]))

(defn reset [ds] (do
  (drop-tables ds)
  (create-tables ds)))

; TODO: take care of sql injection: how?
; Create an account with the given name. Returns the created row as map.
(defn create-account [ds name]
  (sql/insert! ds :account {:name name :balance 0}))

; Returns nil if an account with the given id doesn't exist.
(defn get-account [ds id]
  (sql/get-by-id ds :account id))

(defn deposit [ds id amount]
  (jdbc/execute-one! ds [(str
    "update account"
    " set balance = balance + CAST(" amount " AS money)"
    " where id = " id)]))

; Column names in the database and json names in requests and responses differ.
; These functions help translating between them.
(def table-name-translation
    { :account/id :account-number
    , :account/name :name
    , :account/balance :balance
    })

(defn db-to-json-names [m]
  (rename-keys m table-name-translation))

(defn json-to-db-names [m]
  (rename-keys m (map-invert table-name-translation)))
