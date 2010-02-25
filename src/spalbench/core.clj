(ns spalbench.core
   (:gen-class)
   (:use spalbench.g4file))

(defn -main [& args]
  (read-g4file)
  (println "hoi maailma"))

