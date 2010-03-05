(ns spalbench.plot
  (:use (incanter core stats charts)))

(defn plot-data [x y]
  (view (xy-plot x y)))