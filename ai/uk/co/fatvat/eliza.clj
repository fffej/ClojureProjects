;;; Eliza example from Paradigms of AI Programming (Norvig)
;;; jeff.foster.acm.org
(ns uk.co.fatvat.eliza
  (:use [clojure.contrib.test-is]))

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

(defn variable?
  "Is x a variable (a symbol beginning with '?')?"
  [x]
  (and (symbol? x) (= \? (first (name x)))))

(defn segment-pattern?
  "Is this a segment matching pattern?"
  [pattern]
  (and (sequential? pattern)      
       (sequential? (first pattern))
       (= (ffirst pattern) '?*)))

;; Sentinel value for fail
(def fail {:fail :fail})

;; Indicates pat-match success, with no variables
(def no-bindings {})

(defn match-variable
  "Does var match input? Uses (or updates) and return bindings."
  [var input bindings]
  (let [binding-value (bindings var)]
    (cond
      (nil? binding-value) (assoc bindings var input)
      (= input binding-value) bindings
      :else fail)))

(declare pat-match)

(defn segment-match
  "Match the segment pattern ((?* var) pat) against input"
  ([pattern input bindings]
     (segment-match pattern input bindings 0))
  ([pattern input bindings start]
     (let [var (second (first pattern)) ;; TODO first again?
	   pat (rest pattern)]
       (if (empty? pat)
	 (match-variable var input bindings)
	 ;; We assume that pat starts with a constant
	 ;; In other words, a pattern can't have 2 consecutive vars
	 (let [pos (position (partial = (first pat)) input start)]
	   (if (nil? pos)
	     fail
	     (let [b2 (pat-match pat (map identity (subvec (vec input) pos))
				 (match-variable var (map identity (subvec (vec input) 0 pos)) bindings))]
	       ;; If this match failed, try another longer one
	       (if (= b2 fail)
		 (segment-match pattern input bindings (inc pos))
		 b2))))))))

(defn pat-match 
  "Match pattern against input in the contet of the bindings"
  ([pattern input] 
     (pat-match pattern input no-bindings))
  ([pattern input bindings]
     (cond
       (= bindings fail) fail
       (variable? pattern) (match-variable pattern input bindings)
       (= pattern input) bindings
       (segment-pattern? pattern) (segment-match pattern input bindings)
       (and (sequential? pattern) 
	    (sequential? input)) (pat-match 
				  (rest pattern) (rest input)
				  (pat-match 
				   (first pattern) 
				   (first input) 
				   bindings))
	  :else fail)))

(deftest test-pattern-matching
  (is (= fail (pat-match '(i need a ?X) '(i really need a vacation))))
  (is (= no-bindings (pat-match '(this is easy) '(this is easy))))
  (is (= fail (pat-match '(?X is ?X) '((2 + 2 is 4)))))
  (is (= '{?X (2 + 2)} (pat-match '(?X is ?X) '((2 + 2) is (2 + 2)))))
  (is (= '{?P (Mr Hulot and I) ?X (a vacation)} (pat-match '((?* ?P) need (?* ?X)) 
							   '(Mr Hulot and I need a vacation))))
  (is (= '{?X (1 2 a b)} (pat-match '((?* ?X) a b (?* ?X)) '(1 2 a b a b 1 2 a b)))))

;; Because we don't have dotted pairs, this test will fail.  This will be addressed
;; in improved versions
;;  (is (= '{(?X a long vacation) (?P I)} (pat-match '(?P need . ?X) '(i need a long vacation)))))
