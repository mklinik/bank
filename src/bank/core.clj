(ns bank.core
  (:require [ring.adapter.jetty :refer :all])
  (:require [ring.util.response :as res])
  (:require [ring.util.request :as req])
  (:require [cheshire.core :refer :all]))

(defn handler [request]
  {:status 200
   :headers {"Content-type" "text/html"}
   :body "Hello World"})

(defn what-is-my-ip [request]
  {:status 200
   :headers {"Content-type" "text/html"}
   :body (:remote-addr request)})

(defn request-echo
  ([request respond raise] (respond (request-echo request)))
  ([request] (do
    (println (req/request-url request))
    (->
      ; the request body is a stream, we have to slurp it or else encode can't
      ; handle it. decode after slurp turns it into nested json
      (res/response (encode (update request :body (comp decode slurp))))
      (res/content-type "application/json")))))

(defn run [async]
  (run-jetty
    request-echo
    {:port  3000
     :async? async
     :join? false}))

(defn -main
  [& args]
  (run false))
