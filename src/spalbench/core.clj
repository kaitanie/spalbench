(ns spalbench.core
   (:gen-class)
   (:use spalbench.g4file))

(defn -main [& args]
  (print (first (read-g4file))))

