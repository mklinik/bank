(ns bank.util
  (:require
    [clojure.test :refer :all]))

; https://gist.github.com/Medeah/2bc6e92b86ae7e8abc14
;; A Simple macro that enables you to change your testing groups to pending
(defmacro pending [name & body]
  (let [message (str "PENDING " name)]
    `(testing ~name (println ~message))))
