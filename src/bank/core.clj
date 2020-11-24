(ns bank.core
  (:use compojure.core)
  (:require [ring.adapter.jetty :refer :all])
  (:require [ring.util.response :as res])
  (:require [ring.util.request :as req])
  (:require [ring.middleware.json :refer :all])
  (:require [compojure.route :as route])
  (:require [cheshire.core :as json]))

(defn create-account
  ([request respond raise] (respond (create-account request)))
  ([request] (do
    (println "request account")
    (->
      (res/response (json/encode {"TODO" "implement response"}))
      (res/content-type "application/json")))))

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

(defroutes main-routes
  (ANY "/echo" [] echo)
  (POST "/account" [] (wrap-json-body create-account))
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
