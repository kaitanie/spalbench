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

(defn- field-separator? [char]
  (= char \|))

(defn- parse-token
  "Extract one token separated by separator characters. Returns the
  token and the rest of the sequence."
  [current-line]
  (loop [current-field []
	 current-char (first current-line)
	 remaining-chars (rest current-line)]
    (let [separator? (fn [#^Character char] (or (= \newline char) (Character/isWhitespace char)))]
      (cond
       (or (separator? current-char)
	   (empty? remaining-chars)) [(apply str (conj current-field current-char)) remaining-chars]
       true (recur (conj current-field current-char)
		   (first remaining-chars)
		   (rest remaining-chars))))))

(defn- find-token
  "Skip over whitespace and comment sections. Resturns the next token and the remaining seq."
  [input-seq]
  (loop [remaining input-seq]
    (let [whitespace? (fn [char] (blank-string? (str char)))
	  current-char (first remaining)]
      (cond (or (nil? current-char) (empty? (str current-char))) nil
	    (and (whitespace? current-char) (not (empty? remaining))) (recur (rest remaining))
	    true (let [[token new-remaining] (parse-token remaining)]
		   [token new-remaining])))))

(defn- tokenize-line [line]
  (let [line-seq (seq line)]
    (loop [tokens []
	   remaining line-seq]
      (cond (or (empty? remaining) (= \newline (first remaining))) tokens
	    true (let [[new-token new-remainder] (find-token remaining)]
		   (recur (conj tokens new-token)
			  new-remainder))))))

(defn- parse-header-line
  "Extracts the list of angles."
  [line]
  (let [tokens (tokenize-line line)]
    (loop [angles []
	   current-angle nil
	   in-angle-definition? false
	   remaining-data tokens]
      (let [current-token (first remaining-data)]
	(print (str "Current token = \"" current-token "\" \n"))
	(cond (.contains current-token "|") (recur angles nil in-angle-definition? (rest remaining-data))
	      (.contains current-token "Angle-integrated") angles ;; Terminate the recursion
	      true (recur (conj angles
				{:angle (Double/parseDouble current-token)
				 :label (str current-token (first (rest remaining-data)))})
			  nil
			  in-angle-definition?
			  (rest (rest remaining-data))))))))

(defn parse-data-line [angles line]
  (let [filter-fn (fn [token] (not (. token contains "|")))
	tokens (filter filter-fn (tokenize-line line))
	E (Double/parseDouble (first tokens))
	dE (Double/parseDouble (nth tokens 1))
	angle-data (rest (rest (rest tokens)))]
    (loop [points []
	   remaining-angles angles
	   remaining-tokens angle-data]
      (cond (empty? remaining-angles) points
	    true (let [current-angle (first remaining-angles)]
		   (recur (conj points {:angle (current-angle :angle)
					:label (current-angle :label)
					:E E
					:dE dE
					:cx (Double/parseDouble
					     (first remaining-tokens))
					:dcx (Double/parseDouble
					      (nth remaining-tokens 2))})
				(rest remaining-angles)
				(rest (rest remaining-tokens))))))))

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
	   in-field? false     ;; Are we inside a field?
	   in-whitespace? false ;; Are we swimming in a sea of whitespace?
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
						   (not in-field?)
						   in-whitespace?
						   (first remaining-chars)
						   (rest remaining-chars))
	 ;; Separator can be inside a quoted field
	 (= \" current-char) (if (and (= \" (first remaining-chars)) quoted?)
			       (recur angles
				      (conj current-field \")
				      quoted?
				      in-field?
				      in-whitespace?
				      (first (rest remaining-chars))
				      (rest (rest remaining-chars)))
			       (recur angles
				      current-field
				      (not quoted?)
				      in-field?
				      in-whitespace?
				      (first remaining-chars)
				      (rest remaining-chars)))
	 ;; Otherwise we just take combine the current character with the field
	 ;; and continue start processing the remainder of the string
	 true (recur angles
		     (conj current-field current-char)
		     quoted?
		     in-field?
		     in-whitespace?
		     (first remaining-chars)
		     (rest remaining-chars)))))))

(defn get-data []
  (let [data (line-seq (BufferedReader. (FileReader. "c:/Users/mael/src/spalbench/data/p_Pb_1200_ddxsn_g4bic.txt")))]
    data))

(defmacro nor [& body]
  `(not (or ~@body)))

(defn g4file-read []
  (let [data (get-data)
	header-line (first data)
	angles (parse-header-line header-line)
	filter-fn (fn [x] (nor (. x contains "|")
			       (and (. x contains "-") (. x contains "+"))))
	parser-fn (fn [x] (parse-data-line angles x))
	data-block-length (- (count data) 2 4)
	data-lines (take data-block-length (rest (rest (rest (rest data)))))
        data-tokens (filter filter-fn (map parser-fn data-lines))]
    data-tokens))

(defn read-g4file []
  (let [data (line-seq (BufferedReader. (FileReader. "c:/Users/mael/src/spalbench/data/p_Pb_1200_ddxsn_g4bic.txt")))
	first-line (first data)
	[first-token rest-of-the-data] (parse-token "abc def")
	my-token (find-token "  abc")
	tokens (parse-header-line first-line)
	angles (parse-list-of-angles first-line)
	data-lines (take 20 (rest (rest (rest (rest data)))))
	parser-fn (fn [d] (parse-data-line angles d))
	parsed-data (map parser-fn data-lines)]
    (doseq [d data]
      (print (str "new item: " d "\n")))
    (print (str "first line: " first-line "\n"))
    (doseq [item (parse-list-of-angles first-line)]
      (print item))
     (print (str "\n First token: " first-token "\n"))
     (print (str "\n My-token: " my-token "\n"))
     (print "Tokens:\n")
     (doseq [item tokens]
       (print (str item "\n")))
     (print parsed-data)))

