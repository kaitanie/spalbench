(ns spalbench.core
   (:gen-class)
   (:use spalbench.g4file
	 spalbench.plot))

(defn -main [& args]
  (let [data-map (g4file-read)
	angle2 (map first data-map)
	e-points (map #(% :E) angle2)
	cx-points (map #(% :cx) angle2)]
      (plot-data e-points cx-points)))


