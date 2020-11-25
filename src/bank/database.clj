(ns bank.database
  (:require [next.jdbc :as sql]))

(defn create-tables [db]
  (sql/execute! db ["create table if not exists account"
    "( id serial not null"
    ", name nvarchar not null"
    ", balance money"
    ", primary key (id));"]))
