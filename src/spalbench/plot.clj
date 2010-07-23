(ns spalbench.plot
  (:use (incanter core stats charts latex))
  (:use spalbench.pointfilters)
  (:import (org.jfree.chart.axis LogarithmicAxis NumberAxis)
	   (org.jfree.chart.plot XYPlot)))

(defn test-plot [x y]
  (let [eq (str "f(x)=\\frac{1}{\\sqrt{2\\pi \\sigma^2}}" "e^{\\frac{-(x - \\mu)^2}{2 \\sigma^2}}")]
    (doto (function-plot pdf-normal -3 3)
      (add-latex 0 0.1 eq)
      view)))


(defn log-plot-data [x y label]
  (let [min-x (apply min (filter pos? x))
	min-y (apply min (filter pos? y))
	[filtered-x filtered-y] (handle-zeroes-for-log-log x y)
;;	[filtered-x filtered-y] (filter-zeroes-x-y x y)
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

(defn example []
  (let [d [{:xmin 0.0 :xmax 1.0 :content 1.0}
	   {:xmin 1.0 :xmax 2.0 :content 2.0}]
	mapper-fn (fn [entry] [{:x (:xmin entry) :y (:content entry)}
			       {:x (:xmax entry) :y (:content entry)}])
;;	path (map mapper-fn d)
	x (map #(:x %) path)
	y (map #(:y %) path)]
    path))
;;    [x y]))
    

(defn teststuff [& args]
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
    (clear-background p)
    (view p)
    p))
