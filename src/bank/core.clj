(ns bank.core
  (:use compojure.core)
  (:require [ring.adapter.jetty :refer :all])
  (:require [ring.util.response :as res])
  (:require [ring.util.request :as req])
  (:require [ring.middleware.json :refer :all])
  (:require [compojure.route :as route])
  (:require [cheshire.core :as json])
  (:require [bank.database :as db]))

; For debugging. Don't wrap this in any middleware.
(defn echo
  ([request] (do
    (->
      ; the request body is a stream, we have to slurp it or else encode can't
      ; handle it. decode after slurp turns it into nested json. Beware:
      ; streams can only be slurped once. If any middleware slurps the stream,
      ; it will be nil here. Don't wrap the echo handler in any middleware.
      (res/response (json/encode (update request :body (comp json/decode slurp))))
      (res/content-type "application/json")))))

(defn create-account
  ([request] (do
    (let [new-account-raw (db/create-account db/default-ds (get-in request [:body "name"]))
          new-account (db/db-to-json-names new-account-raw)]
      (->
        (res/response (json/encode new-account))
        (res/content-type "application/json"))))))

(defn retrieve-account
  ([request] (do
    ; TODO: answer with 400 bad request if parseInt fails
    ; TODO: find a way to test status codes; needs some intelligent curl usage
    (if-let [got-account-raw (db/get-account db/default-ds (Integer/parseInt (get-in request [:route-params :id])))]
      (let [got-account (db/db-to-json-names got-account-raw)]
        (->
          (res/response (json/encode got-account))
          (res/content-type "application/json")))
      (res/not-found (json/encode {}))))))

; The amount must be positive. Amount is passed in the json body.
; Ideally we would also verify that the given account id exists, but that is
; implicitly handelled when the deposit command returns nil as result.
(defn verify-deposit-parameters [request]
  (> (get-in request [:body "amount"]) 0))

(defn deposit
  ([request] (do
    (if-let [got-account-raw (db/deposit
                db/default-ds
                (Integer/parseInt (get-in request [:route-params :id]))
                (get-in request [:body "amount"]))]
      (let [got-account (db/db-to-json-names got-account-raw)]
        (->
          (res/response (json/encode got-account))
          (res/content-type "application/json")))
      (res/not-found (json/encode {}))))))

; wrap-json-body must be on the innermost level, because it consumes the :body
; input stream. defroutes tries routes until the first one matches. If multiple
; rules use wrap-json-body on the outside, only the first one gets an actual
; json body, the others get nil. FUCK STATE.
(defroutes main-routes
  (ANY "/echo" [] echo)
  (POST "/account" [] (wrap-json-body create-account))
  (POST "/account/:id/deposit" [] (wrap-json-body deposit))
  (GET "/account/:id" [] retrieve-account)
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
