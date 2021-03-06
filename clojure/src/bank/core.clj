(ns bank.core
  (:use compojure.core)
  (:require [ring.adapter.jetty :refer :all])
  (:require [ring.util.response :as res])
  (:require [ring.util.request :as req])
  (:require [ring.middleware.json :refer :all])
  (:require [compojure.route :as route])
  (:require [cheshire.core :as json])
  (:require [bank.database :as db]))

(defn create-account
  ([request] (do
    (let [new-account (db/create-account db/default-ds (get-in request [:body "name"]))]
      (->
        (res/response (json/encode new-account))
        (res/content-type "application/json"))))))

(defn verify-retrieve-parameters [request]
  (when-let [id-str (get-in request [:route-params :id])]
    (try
      {:id (Integer/parseInt id-str)}
      (catch Exception e nil)))
)

; TODO: write unit test for malformed account id parameter
(defn retrieve-account
  ([request] (do
    ; TODO: find a way to test status codes; needs some intelligent curl usage
    (if-let [params (verify-retrieve-parameters request)]
      (if-let [got-account (db/get-account db/default-ds (:id params))]
        (->
          (res/response (json/encode got-account))
          (res/content-type "application/json"))
        (res/not-found (json/encode {})))
      (res/bad-request (json/encode {}))))))

; The amount must be positive, and id must be a parsable integer. Amount is
; passed in the json body. Ideally we would also verify that the given account
; id exists, but that is implicitly handelled when the deposit command returns
; nil as result.
(defn verify-deposit-parameters [request]
  (when-let [amount (get-in request [:body "amount"])]
  (when-let [id-str (get-in request [:route-params :id])]
    (try
      (when (> amount 0) {:amount amount :id (Integer/parseInt id-str)})
      (catch Exception e nil)))
))

(defn deposit
  ([request] (do
    (if-let [params (verify-deposit-parameters request)]
      (if-let [got-account (db/deposit
                  db/default-ds
                  (:id params)
                  (:amount params))]
        (->
          (res/response (json/encode got-account))
          (res/content-type "application/json"))
        (res/not-found (json/encode {})))
      (res/bad-request (json/encode {}))))))

; The amount must be positive, and id must be a parsable integer. Amount is
; passed in the json body. Ideally we would also verify that the given account
; id exists, but that is implicitly handelled when the deposit command returns
; nil as result.
(defn verify-withdraw-parameters [request]
  (when-let [amount (get-in request [:body "amount"])]
  (when-let [id-str (get-in request [:route-params :id])]
    (try
      (when (> amount 0) {:amount amount :id (Integer/parseInt id-str)})
      (catch Exception e nil)))
))

(defn withdraw
  ([request] (do
    (if-let [params (verify-withdraw-parameters request)]
      (if-let [got-account (db/withdraw
                  db/default-ds
                  (:id params)
                  (:amount params))]
        (->
          (res/response (json/encode got-account))
          (res/content-type "application/json"))
        (res/not-found (json/encode {})))
      (res/bad-request (json/encode {}))))))


(defn verify-transfer-parameters [request]
  (when-let [amount (get-in request [:body "amount"])]
  (when-let [id-str (get-in request [:route-params :id])]
  (when-let [receiver-id (get-in request [:body "account-number"])]
    (try
      (when
        (> amount 0)
        {:amount amount
         :sender-id (Integer/parseInt id-str)
         :receiver-id receiver-id
         })
      (catch Exception e nil))))
))

(defn transfer
  ([request] (do
    (if-let [params (verify-transfer-parameters request)]
      (if-let [got-account (db/transfer
                  db/default-ds
                  (:sender-id params)
                  (:receiver-id params)
                  (:amount params))]
        (->
          (res/response (json/encode got-account))
          (res/content-type "application/json"))
        ; TODO: proper error response. db/transfer can fail for other reasons
        ; than not found. Maybe encode reason in the return value of
        ; db/transfer?
        (res/not-found (json/encode {})))
      (res/bad-request (json/encode {}))))))

(defn get-audit-log
  ([request] (do
    (if-let [params (verify-retrieve-parameters request)]
      (if-let [audit-log (db/get-audit-log db/default-ds (:id params))]
        (->
          (res/response (json/encode audit-log))
          (res/content-type "application/json"))
        (res/not-found (json/encode {})))
      (res/bad-request (json/encode {}))))))

; wrap-json-body must be on the innermost level, because it consumes the :body
; input stream. defroutes tries routes until the first one matches. If multiple
; rules use wrap-json-body on the outside, only the first one gets an actual
; json body, the others get nil. FUCK STATE.
(defroutes main-routes
  (POST "/account" [] (wrap-json-body create-account))
  (POST "/account/:id/deposit" [] (wrap-json-body deposit))
  (POST "/account/:id/withdraw" [] (wrap-json-body withdraw))
  (POST "/account/:id/send" [] (wrap-json-body transfer))
  (GET "/account/:id" [] retrieve-account)
  (GET "/account/:id/audit" [] get-audit-log)
  (route/not-found "Page not found"))

(def app (->
  main-routes))

(defn run [async]
  (run-jetty
    app
    {:port  3000
     :async? async
     :join? false}))

(defn -main
  [& args]
  (run false))
