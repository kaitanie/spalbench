(defproject spalbench "0.0.1-SNAPSHOT"
  :description "Spallation plotting application"
  :repositories {"incanter" "http://repo.incanter.org"}
  :dependencies [[org.clojure/clojure "1.2.0-master-SNAPSHOT"]
                [incanter "1.2.3-SNAPSHOT"]
                [fleetdb "0.2.0-RC1"]
                [org.clojure/clojure-contrib "1.2.0-SNAPSHOT"]]
  :dev-dependencies [[leiningen-run "0.3"]
                     [autodoc "0.7.0"]
                     [leiningen/lein-swank "1.1.0"]]
  :main spalbench.core)
