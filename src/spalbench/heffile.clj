(ns spalbench.heffile
  (:use spalbench.pgf
	clojure.contrib.duck-streams))

(def hef-known-item-types #{:hef-file :hef-histogram})

(defmacro hef-item [type & body]
  `(hash-map :hef-node-type ~type ~@body))

(defmacro hef-file [& body]
  `(hef-item :hef-file ~@body))

(defmacro hef-histogram [& body]
  `(hef-item :hef-histogram ~@body))

(defmacro hef-points [& body]
  `(hef-item :hef-datapoints-2d ~@body))

(defn hef-query-histogram-by-name [hef-data histo-name]
  (let [result (filter #(= (:name %) histo-name) hef-data)
	items (count result)]
    (cond 
     (= items 1) (first result)
     (> items 1) (do (println "More than one result found")
		     result)
     true (do (println (str "Histogram " histo-name " not found."))
	      result))))

(defn make-hef-data-parser [d]
  (fn [] (eval (do
		 (use 'spalbench.heffile)
		 (eval d)))))

(defmulti hef-plot (fn [item] (item :hef-node-type)))

(defn binning->histoprofile [binning]
  (let [low-edges (map #(hash-map :x (:xmin %) :y (:content %)) binning)
	high-edges (map #(hash-map :x (:xmax %) :y (:content %)) binning)]
    (interleave low-edges high-edges)))

(defmethod hef-plot :hef-histogram [d]
  (let [contents (binning->histoprofile (:data d))
	points (map #(str "(" (:x %) "," (:y %) ")\n") contents)
	points-str (apply str points)]
    (addplot "sharp plot" "mark=" contents)))
;;    (pgfplot (axis "x-axis" "y-axis" :loglog
;;		   points-str))))

(defn example-pgf-plot []
  (pgfplot
   (axis "x-axis" "y-axis" :loglog
	 (addplot "plot" "color=red" [{:x 1.0 :y 2.0} {:x 2.0 :y 10.0}])
	 (addplot "plot" "color=black,mark=x" [{:x 1.0 :y 2.0} {:x 2.0 :y 10.0}]))))

(defn composite-log-log-plot [histograms]
  (let [histo-codes (map hef-plot histograms)
	histo-codes-str (reduce #(str % "\n") histo-codes)]
    (pgfplot (axis "Neutron energy $E_n$ [MeV]"
		   "Cross section $\\frac{d \\sigma}{dE d\\Theta}$"
		   :linlin
		   histo-codes-str))))

(defmethod hef-plot :hef-file [d]
  (println "It isn't possible to plot the entire file right now."))

(defn bin->linepoints
  "One bin: {:xmin xmin :xmax xmin :content y} must be
  converted to: [{:x xmin :y y} {:x xmax :y y}]"
  [bin]
  (let [p1 {:x (:xmin bin) :y (:content bin)}
	p2 {:x (:xmax bin) :y (:content bin)}]
    [p1 p2]))

(defn hef-histo->linepoints [histo]
  (let [points (map bin->linepoints (histo :data))]
    (reduce concat points)))

(defn linepoints->columns [linepoints]
  (let [points-str (map (fn [p] (str (:x p) " " (:y p) "\n")) linepoints)]
    (reduce str points-str)))
	

(def example-hef-data (hef-file :comment "C + C @ 135 MeV/A, rel6"
				:format-version 1
				:data [(hef-histogram
					:name "ddxs1"
					:title "My little histogram"
					:comment "(empty comment)"
					:nbins 2
					:binning-type :=>xmin<xmax
					:content [{:xmin 0.0 :xmax 1.0 :content 10.0}
						  {:xmin 1.0 :xmax 2.0 :content 0.0}])]))

(defn hef-read [filename]
  (let [data-str (slurp filename)]
    data-str))

(defn hef-parse [data-str]
  (let [data-expressions (read-string data-str)]
    (fn [] (eval (do
		   (use 'spalbench.heffile)
		   (eval data-expressions))))))
