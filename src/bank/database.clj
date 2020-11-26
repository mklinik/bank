(ns bank.database
  (:require
    [clojure.set :refer [rename-keys, map-invert]]
    [next.jdbc :as jdbc]
    [next.jdbc.result-set :refer [as-unqualified-kebab-maps]]
    [next.jdbc.sql :as sql]))

(def default-db {:dbtype "postgresql" :host "localhost" :dbname "bank" :user "mkl" :password "w00t"})
(def default-ds (jdbc/get-datasource default-db))

(defn drop-tables [ds]
  (do
    (jdbc/execute-one! ds ["drop table if exists account"])
    (jdbc/execute-one! ds ["drop table if exists audit_log"])
    (jdbc/execute-one! ds ["drop table if exists next_sequence_number"])))

(defn create-tables [ds]
  (do
    (jdbc/execute-one! ds [(str
      "create table if not exists account"
      "( account_number serial not null"
      ", name text not null"
      ", balance money"
      ", primary key (account_number))")])
    (jdbc/execute-one! ds [(str
      "create table if not exists audit_log"
      "( sequence_number integer not null"
      ", account_number integer not null"
      ", debit integer default null"
      ", credit integer default null"
      ", description text not null"
      ")"
      )])
    ; We could store it in account directly. This has advantages and
    ; disadvantages. For now, let's make a separate table.
    (jdbc/execute-one! ds [(str
     "create table if not exists next_sequence_number"
     "( account_number integer not null"
     ", next_sequence_number integer not null"
     ")")])
))

(defn reset [ds] (do
  (drop-tables ds)
  (create-tables ds)))

; An audit record is a map with the following keys. Note the underscores in key
; names, they must match the column names in the audit table
;  :account_number int -- the account number this audit record refers to
;  :debit int  -- optional, only if money got deducted in this transaction
;  :credit int -- optional, only if money got added in this transaction
;  :description str -- a description of the transaction as per the specification
;
; This function is supposed to be used within the transaction of one of the
; database operations. No other operation should come between! This also means
; that all transactions should be serializable.
(defn record-audit-log [conn audit-record]
  (let [next-sequence-number (:next_sequence_number/next_sequence_number
    (jdbc/execute-one! conn [(str
    "update next_sequence_number"
    " set next_sequence_number = next_sequence_number + 1"
    " where account_number = ?"
    " returning next_sequence_number") (:account_number audit-record)]))]
    (sql/insert! conn :audit_log (assoc audit-record :sequence_number (- next-sequence-number 1))))
)

; Create an account with the given name. Returns the created row as map. Note:
; this returns the complete row only for postgresql. Other databases may return
; only the keys. See next.jdbc.sql documentation. See also the RETURNING clause.
(defn create-account [ds name]
  ; we need :isolation :serializable because there should not come any
  ; transaction between account creation and next_sequence_number creation
  (jdbc/with-transaction [conn ds {:isolation :serializable}]
    (let [result (sql/insert! conn :account {:name name :balance 0} {:builder-fn as-unqualified-kebab-maps})]
        (sql/insert! conn :next_sequence_number {:account_number (:account-number result) :next_sequence_number 0})
        result)))

; Returns nil if an account with the given id doesn't exist.
(defn get-account [ds id]
  (sql/get-by-id ds :account id "account_number" {:builder-fn as-unqualified-kebab-maps}))

; Deposits the given amount to the account and returns the updated account.
; When a negative amount is given, this is a noop.
(defn deposit [ds id amount]
  ; We're using a transaction to make sure that the read after the update reads
  ; exactly what was updated. No other transaction should come in between.
  ; TODO: the default transaction isolation level of postgres is Read
  ; Committed. Is this enough for our purpose?
  (jdbc/with-transaction [conn ds {:isolation :serializable}]
    ; important! use conn for all executions, not ds
    (when
      (= 1 (:next.jdbc/update-count (jdbc/execute-one! conn [(str
      "update account"
      " set balance = balance + ?::numeric::money"
      " where account_number = ?"
      " and ? > 0") amount id amount])))
      (record-audit-log conn {:account_number id, :credit amount, :description "deposit"}))
    (get-account conn id)))


; The where statement assures that balance can't fall below zero, and that the
; amount is positive.
(defn withdraw [ds id amount]
  (jdbc/with-transaction [conn ds {:isolation :serializable}]
    (when
      (= 1 (:next.jdbc/update-count (jdbc/execute-one! conn [(str
      "update account"
      " set balance = balance - ?::numeric::money"
      " where account_number = ?"
      " and balance >= ?::numeric::money"
      " and ? > 0") amount id amount amount])))
      (record-audit-log conn {:account_number id, :debit amount, :description "withdraw"}))
    (get-account conn id)))


; Returns empty map when parameter verification fails
; TODO: what is a consistent way of handling errors? See
; https://github.com/mklinik/bank/issues/8
(defn transfer [ds sender-id receiver-id amount]
  ; I think we need isolation level serializable in for this transaction, but I
  ; need to contemplate more about isolation levels.
  (try
    (jdbc/with-transaction [conn ds {:isolation :serializable :auto-commit false}]
      (when (< amount 0) (throw (Exception. "amount is negative")))
      ; first check that both accounts exist, and that they are different account numbers
      (let [result (jdbc/execute! conn [(str
        "select account_number from account"
        " where account_number = ?"
        " or account_number = ?") sender-id receiver-id])]
        (when
          (not= 2 (count result))
          (throw (Exception. "account verification failed"))))
      (when
        (< (:balance (get-account conn sender-id)) amount)
        (throw (Exception. "insufficient funds on sender account")))

      ; with all the above checks passing, we can now safely transfer the money
      ; These should not fail, and we can safely create audit logs
      (jdbc/execute-one! conn [(str
        "update account"
        " set balance = balance - ?::numeric::money"
        " where account_number = ?") amount sender-id])
      (jdbc/execute-one! conn [(str
        "update account"
        " set balance = balance + ?::numeric::money"
        " where account_number = ?") amount receiver-id])
      (record-audit-log conn
        {:account_number sender-id
        ,:debit amount
        ,:description (str "send to #" receiver-id)})
      (record-audit-log conn
        {:account_number receiver-id
        ,:credit amount
        ,:description (str "receive from #" sender-id)})
      (get-account conn sender-id))
    (catch Exception e nil)))
