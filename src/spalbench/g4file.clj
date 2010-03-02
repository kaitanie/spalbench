(ns spalbench.g4file
  (:import (java.io BufferedReader FileReader))
  (:use clojure.contrib.duck-streams))

(defn blank-string?
  "Check whether the string is blank or not"
  [#^String s]
  (every? (fn [#^Character c] (Character/isWhitespace c)) s))
 
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

(defn- field-separator? [char]
  (= char \|))

(defn- parse-list-of-angles
  "Extract the list of angles from the first line of the file"
  [line]
  (let [characters (seq line)]
    (loop [angles []           ;; Collect the list of angles in format:
	                       ;; {:angle xx :label "xx degrees"}
	   current-field []    ;; The current position in the string
;;	   current-angle []    ;; The current position in the string
;;	   current-label []    ;; The current position in the label string
	   quoted? false       ;; Are we inside a quoted string?
;;	   in-field? false     ;; Are we inside a field?
;;	   in-whitespace false ;; Are we swimming in a sea of whitespace?
	   current-char (first characters)
	   remaining-chars (rest characters)]
      (let [unquoted-separator? (fn [char] (and (field-separator? char) (not quoted?)))
	    lf? (fn [current-char?] (= \newline current-char))
	    crlf? (fn [current-char remaining-chars] (and (= \return current-char)
							  (= \newline (first remaining-chars))))
	    field-with-remainder (fn [remaining-chars] (vector (conj angles
								     (apply str current-field))
							       remaining-chars))]
	;; Parse different cases:
	(cond
	 ;; We have reached the end of the sequence
	 (nil? current-char) (field-with-remainder nil)
	 ;; End of the line (LF)
	 (and (not quoted?) (lf? current-char)) (field-with-remainder remaining-chars)
	 ;; End of the line (CRLF)
	 (and (not quoted?) (crlf? current-char remaining-chars)) (field-with-remainder (rest remaining-chars))
	 ;; Field separator that is not quoted
	 (unquoted-separator? current-char) (recur (conj angles (apply str current-field))
						   []
						   quoted?
						   (first remaining-chars)
						   (rest remaining-chars))
	 ;; Separator can be inside a quoted field
	 (= \" current-char) (if (and (= \" (first remaining-chars)) quoted?)
			       (recur angles
				      (conj current-field \")
				      quoted?
				      (first (rest remaining-chars))
				      (rest (rest remaining-chars)))
			       (recur angles
				      current-field
				      (not quoted?)
				      (first remaining-chars)
				      (rest remaining-chars)))
	 ;; Otherwise we just take combine the current character with the field
	 ;; and continue start processing the remainder of the string
	 true (recur angles
		     (conj current-field current-char)
		     quoted?
		     (first remaining-chars)
		     (rest remaining-chars)))))))

(defn read-g4file []
  (let [data (line-seq (BufferedReader. (FileReader. "data/p_Pb_1200_ddxsn_g4bic.txt")))
	first-line (first data)]
    (doseq [d data]
      (print (str "new item: " d "\n")))
    (print (str "first line: " first-line "\n"))
    (doseq [item (parse-list-of-angles first-line)]
      (print item))))

;;  (doseq [x (read-and-preprocess-file "data/p_Pb_1200_ddxsn_g4bic.txt")]
;;    (if (not (blank-string? x))
;;	(print (str x "\n")))))
