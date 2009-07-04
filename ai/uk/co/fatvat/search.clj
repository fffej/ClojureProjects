;;; Tree searching examples from Paradigms of AI Programming (Norvig)
;;; jeff.foster.acm.org
(ns uk.co.fatvat.search
  (:use [uk.co.fatvat.debug])
  (:use [clojure.contrib.test-is]))

(defn tree-search
  "Find a state that satisfies goal? Start with states, and search 
   according to successors and combiner"
  [states goal? successors combiner]
  (dbg :search "Search %s" states)
  (cond
    (empty? states) nil
    (goal? (first states)) (first states)
    :else (recur 
	   (combiner (successors (first states)) (rest states))
	   goal?
	   successors
	   combiner)))

(defn depth-first-search
  "Search new states first until goal is reached."
  [start goal? successors]
  (tree-search (list start) goal? successors concat))

(defn reverse-concat
  "Prepend y to start of x"
  [x y]
  (concat y x))

(defn breadth-first-search 
  "Search old states first until goal is reached."
  [start goal? successors]
  (tree-search (list start) goal? successors reverse-concat))

(defn sorter
  "Return a combiner function that sorts according to cost-fn"
  [cost-fn]
  (fn [new old]
    (sort (fn [n o] (< (cost-fn n) (cost-fn o))) (concat new old))))

(defn best-first-search
  "Search lowest cost states first until goal is reached"
  [start goal? successors cost-fn]
  (tree-search (list start) goal? successors (sorter cost-fn)))

(defn beam-search
  "Search highest scoring states first until goal is reached"
  [start goal? successors cost-fn beam-width]
  (tree-search (list start) goal? successors
	       (fn [old new]
		 (let [sorted ((sorter cost-fn) old new)]
		   (if (> beam-width (count sorted))
		     sorted
		     (take beam-width sorted))))))

(defn iter-wide-search
  "Search, increasing beam width from width to max.
   Return the first solution found at any width"
  [start goal? successors cost-fn width max]
  (dbg :search (format "Width: %s" width))
  (when-not (> width max)
    (or (beam-search start goal? successors cost-fn width)
	(recur start goal? successors cost-fn (inc width) max))))

;; Searching graphs
(defn new-states
  "Generate successor states that have not been seen before."
  [states successors state-eq old-states]
  (remove
   (fn [state]
      (or (some (partial state-eq state) old-states)
	  (some (partial state-eq state) states)))
   (successors (first states))))

(defn graph-search
  "Find a state that statisfies goal?.  Start with states and search
   according to successors and combiner.  Don't repeat same state twice"
  ([states goal? successors combiner]
     (graph-search states goal? successors combiner = #{}))
  ([states goal? successors combiner old-states]
     (graph-search states goal? successors combiner = old-states))
  ([states goal? successors combiner state-eq old-states]
     (dbg :search "Search: %s" states)
     (cond
       (empty? states) nil
       (goal? (first states)) (first states)
       :else (recur
	      (combiner (new-states states successors state-eq old-states)
			(rest states))
	      goal? successors combiner state-eq
	      (conj old-states (first states))))))
      
(defn next2
  [x]
  (list (+ x 1) (+ x 2)))
  



;;; As an example of tree search, let's consider darts.
(defstruct game :current-score :throws)

(def darts
     (concat
      (range 1 21)
      (map (partial * 2) (range 1 21))
      (map (partial * 3) (range 1 21))
      '(25 50)))
      
(def finishes
     (set (concat (map (partial * 2) (range 1 21)) '(25 50))))

(defn next-throw
  "Given a value return the next valid darts"
  [n]
  (filter
   (partial not= 1)
   (filter 
    (partial <= 0)
    (map (fn [x] 
	   (let [result (- n x)]
	     (if (not= result 0)
	       result
	       (if (finishes x) 0 -1))))
	 darts))))

(defn next-dart
  "Given a value, return the next darts and record the state"
  [d]
  (map (fn [new-score]
	 (struct game 
		 new-score
		 (conj (:throws d) new-score)))
       (next-throw (:current-score d))))

(defn finished? [d] (zero? (:current-score d)))

(defn solve-darts-depth-first
  [n]
  (depth-first-search 
   (struct game n []) 
   finished?
   next-dart))

;; TODO why does this suffer from a stack overflow?
;; It should be slower yes, but break?
(defn solve-darts-breadth-first
  [n]
  (breadth-first-search (struct game n []) finished? next-dart))

(defn solve-darts-beam-search
  [n]
  (beam-search
   (struct game n [])
   finished?
   next-dart
   (fn [d] (/ (- (:current-score d) n) (count (:throws d))))
   3))

(defn solve-darts-iter
  [n]
  (iter-wide-search 
   (struct game n [])
   (fn [d] (zero? (:current-score d)))
   next-dart
   (fn [d] (/ (- (:current-score d) n) (count (:throws d))))
   1
   100))
 