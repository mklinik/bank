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
