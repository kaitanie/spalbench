(ns spalbench.core
   (:gen-class)
   (:use spalbench.heffile
	 clojure.contrib.duck-streams))

(defn process-hef-file [file]
  (let [hef-file-data ((hef-parse (hef-read file)))
	histos (hef-file-data :data)]
    (doseq [histo histos]
      (let [name (histo :name)
	    outfile-name (str name ".dat")
	    linepoints (hef-histo->linepoints histo)
	    header (str "!Histogram: " name "\n")]
	(spit outfile-name (str header
				(linepoints->columns linepoints)))))))

(defn -main [& args]
;;    (do
;;      (println (str (count args) " arguments given:"))
;;      (println args))
  (if (< (count args) 1)
    (println "No args. Please gia HEF-file as an argument")
    (let [file (first args)]
      (process-hef-file file))))
;;    (let [file (nth args 1)]
;;      (process-hef-file file))))