(ns spalbench.plot
  (:use (incanter core stats charts latex))
  (:import (org.jfree.chart.axis LogarithmicAxis NumberAxis)
	   (org.jfree.chart.plot XYPlot)))

(defn test-plot [x y]
  (let [eq (str "f(x)=\\frac{1}{\\sqrt{2\\pi \\sigma^2}}" "e^{\\frac{-(x - \\mu)^2}{2 \\sigma^2}}")]
    (doto (function-plot pdf-normal -3 3)
      (add-latex 0 0.1 eq)
      view)))

(defn filter-2d-points [predicate x y]
  (loop [new-x []
	 new-y []
	 remaining-x x
	 remaining-y y]
    (let [current-x (first remaining-x)
	  current-y (first remaining-y)]
      (cond (empty? remaining-x) [new-x new-y]
	    true (if (predicate current-x current-y)
		   (recur new-x new-y (rest remaining-x) (rest remaining-y))
		   (recur (conj new-x current-x)
			  (conj new-y current-y)
			  (rest remaining-x)
			  (rest remaining-y)))))))

(defn filter-zeroes-y [x-values y-values]
  (filter-2d-points (fn [x y] (<= y 0)) x-values y-values))

(defn filter-zeroes-x-y [x-values y-values]
  (filter-2d-points (fn [x y] (or (<= x 0) (<= y 0))) x-values y-values))

(defn log-plot-data [x y label]
  (let [[filtered-x filtered-y] (filter-zeroes-x-y x y)
	p (xy-plot filtered-x filtered-y :legend true :title "Neutron ddxs"
		   :series-label label)
	range-axis (LogarithmicAxis. "Cross section (mb)")
	domain-axis (LogarithmicAxis. "E (MeV)")]
    (.setRangeAxis (.getPlot p) range-axis)
    (.setDomainAxis (.getPlot p) domain-axis)
    p))

(defn plot-data [x y label]
  (let [p (xy-plot x y :legend true :title "Neutron DDXS cross-sections"
		   :series-label label)]
    p))
