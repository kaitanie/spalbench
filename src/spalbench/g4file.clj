(ns spalbench.g4file
  (:use clojure.contrib.duck-streams))

(defn- read-ddxs-datafile
  "Read Geant4 double-differential spectrun file into a lazy sequence."
  [f]
  (read-lines f))

(defn- remove-pipe-chars
  "Combine the elements of the sequence into a single string and
  remove all pipe (|) characters."
  [f]
  (map (comp #(apply str %)
	     (fn [s] (remove #(= \| %) s)))
       (read-ddxs-datafile f)))

(defn- split-lines
  "Trim double spaces and replace them with single spaces"
  [s]
  (.split (.replaceAll s "  " " ") " "))

(defn- read-and-preprocess-file
  "Return a string that contains the contents of the file"
  [f]
  (split-lines (apply str (interpose " " (remove-pipe-chars f)))))

(defn read-g4file []
  (read-and-preprocess-file "data/p_Pb_1200_ddxsn_g4bic.txt"))
