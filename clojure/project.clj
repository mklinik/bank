(defproject bank "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [ring/ring-core "1.8.2"]
                 [ring/ring-devel "1.8.2"]
                 [ring/ring-json "0.5.0"]
                 [ring/ring-jetty-adapter "1.8.2"]
                 [compojure "1.6.2"]
                 [seancorfield/next.jdbc "1.1.613"]
                 [org.postgresql/postgresql "42.2.18.jre7"]
                 [camel-snake-kebab "0.4.2"]
                 [cheshire "5.10.0"]]
  :plugins [[lein-ring "0.12.5"]]
  :ring {:handler bank.core/app}
  :main bank.core
  :repl-options {:init-ns bank.core})
