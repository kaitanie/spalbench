(defproject spalbench "1.0.0-SNAPSHOT"
  :description "Spallation plotting application"
  :repositories {"incanter" "http://repo.incanter.org"}
  :dependencies [[org.clojure/clojure "1.1.0"]
                [org.incanter/incanter-full "1.0.0"]
                [org.clojure/clojure-contrib "1.1.0"]]
  :dev-dependencies [[leiningen-run "0.3"]
                     [leiningen/lein-swank "1.1.0"]]
  :main spalbench.core)
