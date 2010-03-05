(ns spalbench.core
   (:gen-class)
   (:use spalbench.g4file
	 spalbench.plot))

(defn -main [& args]
  (do
    (plot-data [1.0 2.0 3.0] [1.0 2.0 3.0])
    (print (first (read-g4file)))))

