(ns bank.core
  (:require [ring.adapter.jetty :refer :all]))

(defn handler [request]
  {:status 200
   :headers {"Content-type" "text/html"}
   :body "Hello World"})

(defn what-is-my-ip [request]
  {:status 200
   :headers {"Content-type" "text/html"}
   :body (:remote-addr request)})

(defn run []
  (run-jetty
    what-is-my-ip
    {:port  3000
     :join? false}))
