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
    (println "create account for" (get-in request [:body "name"]))
    (->
      (res/response (json/encode
        {"account-number" 1
         "balance" 0
         "name" (get-in request [:body "name"])}))
      (res/content-type "application/json")))))

(defn retrieve-account
  ([request] (do
    (println "retrieve account" (get-in request [:route-params :id]))
    (->
      (res/response (json/encode {"TODO" "implement retrieve" "id" (get-in request [:route-params :id])}))
      (res/content-type "application/json")))))

(defn deposit
  ([request] (do
    (println "deposit to" (get-in request [:route-params :id]) (get-in request [:body]))
    (->
      (res/response (json/encode {"TODO" "implement deposit"}))
      (res/content-type "application/json")))))

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
