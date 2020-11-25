(ns bank.database
  (:require
    [next.jdbc :as jdbc]
    [next.jdbc.sql :as sql]))

(def default-db {:dbtype "postgresql" :host "localhost" :dbname "bank" :user "mkl" :password "w00t"})
(def default-ds (jdbc/get-datasource default-db))

(defn drop-tables [ds]
  (jdbc/execute! ds ["drop table if exists account"]))

(defn create-tables [ds]
  (jdbc/execute! ds [(str
    "create table if not exists account"
    "( id serial not null"
    ", name text not null"
    ", balance money"
    ", primary key (id))")]))

; TODO: take care of sql injection: how?
(defn create-account [ds name]
  (sql/insert! ds "account" {:name name :balance 0}))

(defn get-account [ds id]
  (sql/get-by-id ds "account" id))
