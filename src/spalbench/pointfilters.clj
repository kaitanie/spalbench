(ns spalbench.pointfilters)

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

(defn min-filter-2d-points [predicate x y]
  (let [le-zero? #(>= % 0.0)
	x-filtered (filter le-zero? x)
	y-filtered (filter le-zero? y)]
    (if (and (> (count x-filtered) 0)
	     (> (count y-filtered) 0))
	(let [min-x (apply min x-filtered)
	      min-y (apply min x-filtered)]
	  (loop [
		 new-x []
		 new-y []
		 remaining-x x
		 remaining-y y]
	    (let [current-x (first remaining-x)
		  current-y (first remaining-y)]
	      (cond (empty? remaining-x) [new-x new-y]
		    true (if (predicate current-x current-y)
			   (recur (conj new-x (/ min-x 1000))
				  (conj new-y (/ min-y 1000))
				  (rest remaining-x) (rest remaining-y))
			   (recur (conj new-x current-x)
				  (conj new-y current-y)
				  (rest remaining-x)
				  (rest remaining-y)))))))
	(do
	  (println (str "count x = " (count x-filtered)))
	  (filter-2d-points predicate x y)))))

(defn filter-zeroes-y [x-values y-values]
  (filter-2d-points (fn [x y] (<= y 0)) x-values y-values))

(defn filter-zeroes-x-y [x-values y-values]
  (filter-2d-points (fn [x y] (or (<= x 0) (<= y 0))) x-values y-values))

(defn filter-trailing-zero-points [x]
  (loop [new-x []
	 remaining-x x]
    (println (str "new-x: " (apply str new-x)))
    (println (str "remaining-x: " (apply str remaining-x)))
    (cond (every? #(<= % 0.0) remaining-x) new-x
	  true (recur (conj new-x (first remaining-x))
		      (rest remaining-x)))))

(defn handle-zeroes-for-log-log [x y]
  (let [filter-fn #(if (<= % 0.0) 1.0e-6 %)
	new-x (map filter-fn x)
	new-y (map filter-fn y)]
    [new-x new-y]))
