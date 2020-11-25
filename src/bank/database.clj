(ns bank.database
  (:require [next.jdbc :as sql]))

(def default-db {:dbtype "postgresql" :host "localhost" :dbname "bank" :user "mkl" :password "w00t"})
(def default-ds (sql/get-datasource default-db))

(defn drop-tables [ds]
  (sql/execute! ds ["drop table if exists account"]))

(defn create-tables [ds]
  (sql/execute! ds [(str
    "create table if not exists account"
    "( id serial not null"
    ", name text not null"
    ", balance money"
    ", primary key (id))")]))

; TODO: take care of sql injection: how?
(defn create-account [ds name]
  (sql/execute! ds [(str
    "insert into account values"
    "(DEFAULT"
    ",'" name "'"
    ", 0)")]))

(defn get-account [ds id]
  (sql/execute! ds [(str
    "select * from account where id = '" id "'")]))
