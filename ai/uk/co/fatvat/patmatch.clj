;;; jeff.foster@acm.org
(ns uk.co.fatvat.patmatch
  (:use [clojure.contrib.def]))

(defvar fail {:fail :fail} "Sentinel value marking no bindings")

(defvar no-bindings {} "Successful match with no bindings")

(declare variable? match-variable segment-pattern? segment-matcher
	 single-pattern? single-matcher)

(defn pat-match
  "Match pattern against input in the context of bindings"
  ([pattern input]
     (pat-match pattern input no-bindings))
  ([pattern input bindings]
     (cond
       (= bindings fail) fail
       (variable? pattern) (match-variable pattern input bindings)
       (= pattern input) bindings
       (segment-pattern? pattern) (segment-matcher pattern input bindings)
       (single-pattern? pattern) (single-matcher pattern input bindings)
       (and
	(sequential? pattern)
	(sequential? input)) (recur (rest pattern) (rest input)
				    (pat-match (first pattern) (first input)
					       bindings))
       :else fail)))

(defn position
  "First index within the sequence satisfying test"
  ([test coll]
     (position test coll 0))
  ([test coll start]
     (position test (nthnext coll start) start start))
  ([test coll start current-pos]
     (cond
       (empty? coll) nil
       (test (first coll)) current-pos
       :else (recur test (rest coll) start (inc current-pos)))))

(defn match-variable
  "Does var match input? Uses (or updates) and return bindings."
  [var input bindings]
  (let [binding-value (bindings var)]
    (cond
      (nil? binding-value) (assoc bindings var input)
      (= input binding-value) bindings
      :else fail)))

(defn first-match-pos
  "Find the first position that pat1 could possibly match input,
   starting at position start.  IF pat1 is non-constant, then jsut
   return start."
  [pat1 input start])

(defn segment-match
  "Match the segment patter ((?* var ) . pat) against input."
  ([pattern input bindings]
     (segment-match pattern input bindings 0))
  ([pattern input bindings start]
     (let [var (second (first pattern))
	   pat (rest pattern)]
       (if (empty? pat)
	 (match-variable var input bindings)
	 (let [pos (first-match-pos (first pat) input start)]
	   (if (nil? pos)
	     fail
	     (let [b2 (pat-match pat (map identity (subvec (vec input) pos))
				 (match-variable var (map identity (subvec (vec input) 0 pos)) bindings))]
	       (if (= b2 fail)
		 (segment-match pattern input bindings (inc pos))
		 b2))))))))

(defn segment-match+ 
  "Match one or more elements of input."
  [pattern input bindings]
  (segment-match pattern input bindings 1))

(defn segment-match?
  "Match zero or one element of input"
  [pattern input bindings]
  (let [var (second (first pattern))
	pat (rest pattern)]
    (or (pat-match (cons var pat) input bindings)
	(pat-match pat input bindings))))

(defn match-is
  "Succeed and bind var if the input satisfies pred,
   where var-and-pred is the list (var pred)."
  [[var pred?] input bindings]
  (let [new-bindings (pat-match var input bindings)]
    (if (or (= new-bindings fail)
	    (not (pred? input)))
      fail
      new-bindings)))

(defn match-and
  "Succeed if all the patterns match the input."
  [patterns input bindings]
  (cond
    (= bindings fail) fail
    (empty? patterns) bindings
    :else (recur (rest patterns) input
		 (pat-match (first patterns) input bindings))))

(defn match-or
  "Succeed if any one of the patterns match the input."
  [patterns input bindings]
  (if (empty? patterns)
    fail
    (let [new-bindings (pat-match (first patterns) input bindings)]
      (if (= new-bindings fail)
	(recur (rest patterns) input bindings)
	new-bindings))))

(defn match-not
  "Succeed if none of the patterns match the input.
   This will never bind any variables."
  [patterns input bindings]
  (if (match-or patterns input bindings)
    fail
    bindings))

(defn match-if
  "Test an arbitrary expression involving variables."
  [pattern input bindings])
  ;; ProgV doesn't have a direct replacement.  Will ponder

(defvar
  dispatch-table
  {:single-match 
   {'?is match-is
    '?or match-or
    '?and match-and
    '?not match-not}
   :segment-match 
   {'?* segment-match
    '?+ segment-match+
    '?? segment-match?
    '?if match-if}}
  "Dispatch table")

(defn single-match-fn
  "Get the single-match function for x, 
  if it is a symbol that has one."
  [x]
  (when (symbol? x) ((dispatch-table :single-match) x)))

(defn segment-match-fn
  "Get the segment-match function for x, 
  if it is a symbol that has one."
  [x]
  (when (symbol? x) ((dispatch-table :segment-match) x)))

(defn variable?
  "Is x a variable (a symbol beginning with '?')?"
  [x]
  (and (symbol? x) (= \? (first (name x)))))

(defn single-pattern?
  "Is this a single-matching pattern?
  E.g. (?is x predicate) (?and . patterns) (?or . patterns)."
  [pattern]
  (and (sequential? pattern)
       (single-match-fn (first pattern))))

(defn segment-pattern?
  "Is this a segment-matching pattern like ((?* var) . pat)?"
  [pattern]
  (and (sequential? pattern) 
       (sequential? (first pattern))
       (symbol? (first (first pattern)))
       (segment-match-fn (first (first pattern)))))



	   
  