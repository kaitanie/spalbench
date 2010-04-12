(defproject spalbench "0.0.1-SNAPSHOT"
  :description "Spallation plotting application"
  :repositories {"incanter" "http://repo.incanter.org"}
  :dependencies [[org.clojure/clojure "1.1.0"]
                [org.incanter/incanter-full "1.0.0"]
                [fleetdb "0.1.1-SNAPSHOT"]
                [org.clojure/clojure-contrib "1.1.0"]]
  :dev-dependencies [[leiningen-run "0.3"]
                     [autodoc "0.7.0"]
                     [leiningen/lein-swank "1.1.0"]]
  :main spalbench.core)
