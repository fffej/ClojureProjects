;;; Tree searching examples from Paradigms of AI Programming (Norvig)
;;; jeff.foster.acm.org
(ns uk.co.fatvat.search
  (:use [uk.co.fatvat.debug])
  (:use [clojure.contrib.test-is]))

(defn tree-search
  "Find a state that satisfic goal? Start with states, and search 
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

(defn binary-tree
  [x]
  (list (* 2 x) (+ 1 (* 2 x))))

(defn finite-binary-tree
  "Return a successor function that generates a binary tree 
   with n nodes"
  [n]
  (fn [x] (filter (partial > n) (binary-tree x))))

(defn reverse-concat
  "Prepend y to start of x"
  [x y]
  (concat y x))

(defn breadth-first-search 
  "Search old states first until goal is reached."
  [start goal? successors]
  (tree-search (list start) goal? successors reverse-concat))

(defn diff
  "Return the function that finds the difference from num."
  [num]
  (fn [x] (Math/abs (- x num))))

(defn sorter
  "Return a combiner function that sorts according to cost-fn"
  [cost-fn]
  (fn [new old]
    (sort (fn [n o] (< (cost-fn n) (cost-fn o))) (concat new old))))

(defn best-first-search
  "Search lowest cost states first until goal is reached"
  [start goal? successors cost-fn]
  (tree-search (list start) goal? successors (sorter cost-fn)))

(defn price-is-right 
  "Return a function that measures the difference from price, but
  gives a big penalty for doing over price."
  [price]
  (fn [x] (if (> x price)
    (Integer/MAX_VALUE)
    (- price x))))

(defn beam-search
  "Search highest scoring states first until goal is reached"
  [start goal? successors cost-fn beam-width]
  (tree-search (list start) goal? successors
	       (fn [old new]
		 (let [sorted ((sorter cost-fn) old new)]
		   (if (> beam-width (count sorted))
		     sorted
		     (take beam-width sorted))))))
  
;;; As an example of search, let's consider chess and in particular
;;; the case where we have a queen + king against a single king.
;;; What's the best route to mate?

(defn add-vec
  [pos delta]
  (map + pos delta))

;; A board consists of the pieces on it
(defstruct board :white-queen :white-king :black-king :white-turn?)

;; Valid moves that a queen or king can make
(def directions
     #{[1 1] [1 0] [1 -1] [0 1] [0 -1] [-1 1] [-1 0] [-1 -1]})

(defn valid-position?
  "Is the supplied position on the chess board?"
  [pos]
  (let [x (first pos) y (second pos)]
    (and (>= x 0) (< x 8) (>= y 0) (< y 8))))

(defn queen-checked?
  "Does the queen check the king?"
  [[qx qy] [kx ky]]
  (or (= qx kx) 
      (= qy ky)
      (= (Math/abs (- qx kx)) (Math/abs (- qy ky)))))

(defn adjacent?
  "Are the two pieces next to each other?"
  [[x1 y1] [x2 y2]]
  (let [d1 (Math/abs (- x1 x2))
	d2 (Math/abs (- y1 y2))]
    (and (<= d1 1) (<= d2 1))))

(defn queen-moves
  [queen]
  (set
   (mapcat
    (fn [delta] (take-while valid-position? (iterate (partial add-vec delta) queen)))
    directions)))

(defn valid-board?
  [board]
  (let [wk (:white-king board)
	wq (:white-queen board)
	bk (:black-king board)]
    (and
     (every? valid-position? [wk wq bk])
     (not (queen-checked? wq bk))
     (not (adjacent? wk bk))
     (= 3 (count #{wk wq bk})))))

(defn next-moves
  "The set of next states"
  [b]
  (let [wk (:white-king b)
	wq (:white-queen b)
	bk (:black-king b)]
    (filter valid-board?
	    (if (:white-turn? b)
	      (concat
	       (map (fn [x] (struct board x wk bk false)) (queen-moves wq))
	       (map (fn [x] (struct board (add-vec wk x) wq bk false)) directions))
	      (map (fn [x] (struct board wk wq (add-vec bk x) true)) directions)))))
		    
(defn check-mate?
  [board]
  (let [wk (:white-king board)
	wq (:white-queen board)
	bk (:black-king board)]
    (println "BOARD=" board)
    (if (:white-turn? board)
      false
      (and
       (queen-checked? wq bk)
       (every? (partial queen-checked? wq)
	       (filter valid-position? (map (partial add-vec bk) directions)))))))

(defn calculate-mate
  [b]
  (breadth-first-search b check-mate? next-moves))
	     


