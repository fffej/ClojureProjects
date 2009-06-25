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

;; A board consists of the pieces on it
(defstruct position :x :y)

(defstruct board :white-queen :white-king :black-king :white-turn?)

(def moves
     #{[1 1] [1 0] [1 -1] [0 1] [0 -1] [-1 1] [-1 0] [-1 -1]})
     
(defn checked-or-invalidmove?
  [white-king white-queen black-king]
  true)

(defn add-vec
  [pos delta]
  (map + pos delta))

(defn mate?
  [board]
  (if (:white-turn? board)
    false
    (every? (partial 
	     checked-or-invalidmove?
	     (:white-king board)
	     (:white-queen board))
	    (map (partial add-vec (:black-king board)) moves))))
	     


