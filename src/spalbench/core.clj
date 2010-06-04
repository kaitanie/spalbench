(ns spalbench.core
   (:gen-class)
   (:use spalbench.g4file
	 spalbench.plot
	 (incanter core stats charts latex)))

(defn extract-points [d entry]
  (let [selector (fn [x] (nth x entry))
	data-for-entry (map selector d)
	x-points (map #(% :E) data-for-entry)
	y-points (map #(% :cx) data-for-entry)
	label ((first data-for-entry) :label)]
    [x-points y-points label]))

(defn plot-points [p [x y label]]
  (doto p
    (add-lines x y :series-label label)))

(defn log-plot-points [p [x y label]]
  (let [[filtered-x filtered-y] (filter-zeroes-x-y x y)]
    (plot-points p [filtered-x filtered-y label])))

(defn -main [& args]
  (let [data-map (g4file-read)
	angle2 (map first data-map)
	e-points (map #(% :E) angle2)
	cx-points (map #(% :cx) angle2)
	[ep cxp label] (extract-points data-map 0)
	p (log-plot-data ep cxp label)]
    (log-plot-points p (extract-points data-map 1))
    (log-plot-points p (extract-points data-map 2))
    (log-plot-points p (extract-points data-map 3))
    (log-plot-points p (extract-points data-map 4))
    (log-plot-points p (extract-points data-map 5))
    (view p)))