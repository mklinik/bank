(ns bank.core
  (:use compojure.core)
  (:require [ring.adapter.jetty :refer :all])
  (:require [ring.util.response :as res])
  (:require [ring.util.request :as req])
  (:require [ring.middleware.json :refer :all])
  (:require [compojure.route :as route])
  (:require [cheshire.core :as json]))

; For debugging. Don't wrap this in any middleware.
(defn echo
  ([request respond raise] (respond (echo request)))
  ([request] (do
    (->
      ; the request body is a stream, we have to slurp it or else encode can't
      ; handle it. decode after slurp turns it into nested json. Beware:
      ; streams can only be slurped once. If any middleware slurps the stream,
      ; it will be nil here. Don't wrap the echo handler in any middleware.
      (res/response (json/encode (update request :body (comp json/decode slurp))))
      (res/content-type "application/json")))))

(defn create-account
  ([request respond raise] (respond (create-account request)))
  ([request] (do
    (println "create account for" (get-in request [:body "name"]))
    (->
      (res/response (json/encode {"TODO" "implement response"}))
      (res/content-type "application/json")))))

(defn retrieve-account
  ([account-id request] (do
    (println "retrieve account" account-id)
    (->
      (res/response (json/encode {"TODO" "implement response" "id" account-id}))
      (res/content-type "application/json")))))

(defn deposit
  ([account-id request] (do
    (println "deposit")
    (->
      (res/response (json/encode {"TODO" "implement response"}))
      (res/content-type "application/json")))))

(defroutes main-routes
  (ANY "/echo" [] echo)
  (wrap-json-body (POST "/account" [] create-account))
  (GET "/account/:id" [id] (partial retrieve-account id))
  (wrap-json-body (POST "/account/:id/deposit" [id] (partial deposit id)))
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
