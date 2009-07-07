;;; Tree searching examples from Paradigms of AI Programming (Norvig)
;;; jeff.foster.acm.org
(ns uk.co.fatvat.search
  (:use [uk.co.fatvat.debug])
  (:use [clojure.contrib.test-is])
  (:import (javax.swing JFrame JPanel))
  (:import (java.awt.event ActionListener MouseListener MouseAdapter MouseEvent))
  (:import (java.awt Color)))

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

;;; Implementation of A* search algorithm
(defstruct path :state :previous :cost-so-far :total-cost)

(defn path-to-string
  [path]
  (format "Path to %s, cost %s" (:state path) (:total-cost path)))

(defn make-path
  "Create a new path object"
  [state previous cost-so-far total-cost]
  (struct path state previous cost-so-far total-cost))

(defn find-path
  "Find the path with this state amongst a list of paths"
  [state paths state-eq]
  (let [x (filter (fn [path] (state-eq (:state path) state)) paths)]
    (when-not (empty? x)
      (first x))))

(defn better-path?
  "Is path1 cheaper than path2?"
  [path1 path2]
  (< (:total-cost path1) (:total-cost path2)))

;; TODO a bit inefficient!
(defn insert-path
  [path paths]
  "Put path in the right position, sorted by total cost."
  (sort better-path? (cons path paths)))

;; TODO use recur
(defn path-states
  "Collect the states along this path."
  [path]
  (when-not (nil? path)
    (cons (:state path) (path-states (:previous path)))))

(defn setf [atom val]
  (swap! atom (constantly val)))
  
(defn a*-search
  "Find a path whose state satisfies goal?.  Start with paths, and expand
   successors, exploring least cost first.  When there are duplicate states,
   keep the one with the lower cost and discard the other."
  ([paths goal? successors cost-fn cost-left-fn]
     (a*-search paths goal? successors cost-fn cost-left-fn = #{}))
  ([paths goal? successors cost-fn cost-left-fn state-eq]
     (a*-search paths goal? successors cost-fn cost-left-fn state-eq #{}))
  ([paths goal? successors cost-fn cost-left-fn state-eq old-paths]
     (dbg :search ";; Search: %s" paths)
     (cond
       (empty? paths) nil
       (goal? (:state (first paths))) (first paths)
       :else (let [path (first paths)
		   rest-paths (rest paths)
		   old-paths-a (atom (insert-path path old-paths)) ;; mutable wrappers
		   paths-a (atom rest-paths)
		   state (:state path)]
	       (doseq [state2 (successors state)]
		 (let [cost (+ (:cost-so-far path)
			       (cost-fn state state2))
		       cost2 (cost-left-fn state2)
		       path2 (make-path state2 path cost (+ cost cost2))
		       old-a (atom nil)]
		   (cond
		     (not (empty? (setf old-a (find-path state2 @paths-a state-eq))))
  		           (when (better-path? path2 @old-a)
			     (setf paths-a (insert-path path2 (remove (partial = @old-a) @paths-a))))
		     (not (empty? (setf old-a (find-path state2 @old-paths-a  state-eq))))
		           (when (better-path? path2 @old-a)
			     (setf paths-a (insert-path path2 @paths-a))
			     (setf old-paths-a (remove (partial = @old-a) @old-paths-a)))
		     :else (setf paths-a (insert-path path2 @paths-a)))))
	       (recur @paths-a goal? successors cost-fn cost-left-fn state-eq @old-paths-a)))))

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
 
;; A* Search Algorithm used to search a maze.
(defstruct point :x :y)

(defn make-cost-and-goal-fn 
  "Make a goal function so the point reaches the goal"
  [x y]
  [(fn [g]
     (let [dx (- x (:x g)) dy (- y (:y g))]
       (Math/sqrt (+ (* dx dx) (* dy dy)))))
   (fn [g]
     (and (= (:x g) x)
	  (= (:y g) y)))])

(defn make-point
  [x y]
  (struct point x y))

(defn make-successors-fn
  "Make a successors function with chance of a surrounding wall"
  [wall]
  (fn [p]
    (let [neighbours #{[1 0] [-1 0] [0 1] [0 -1]}]
      (remove 
       (fn [_] (> wall (rand)))
       (map 
	(fn [[x y]] (make-point (+ x (:x p)) (+ y (:y p))))
	neighbours)))))

(defn search-maze-basic
  [start goal]
  (let [[costf goal?] (make-cost-and-goal-fn (:x goal) (:y goal))]
    (graph-search (list start) goal? (make-successors-fn 0) concat)))

(defn search-maze-a*
  [start goal]
  (let [[costf goal?] (make-cost-and-goal-fn (:x goal) (:y goal))]
    (a*-search (list (make-path start [] 0 0)) goal? (make-successors-fn 0) (constantly 1) costf)))

(def grid-size 32)

;; A collection of walls 
(def walls (atom #{}))

(def canvas 
     (proxy [JPanel] [] 
       (paintComponent 
	[g]
     (proxy-super paintComponent g)
     (let [sq-size (/ (min (.getHeight this) (.getWidth this)) grid-size)]
       (doseq [x (range 0 grid-size)]
	 (doseq [y (range 0 grid-size)]
	   (println x y)
	   (cond 
	     (= [0 0] [x y]) (.setColor g Color/GREEN)
	     (= [(dec grid-size) (dec grid-size)] [x y]) (.setColor g Color/RED)
	     (@walls [x y]) (.setColor g Color/BLACK)
	     :else (.setColor g Color/BLUE))
	   (doto g
	     (.fillRect (* x sq-size) (* y sq-size) (dec sq-size) (dec sq-size)))))))))

(defn visualize
  []
  "A quick and dirty visualization of A* search on a 100 x 100 grid"
  (let [frame (JFrame. "A* Search Algorithm Visualization")
	[costf goal?] (make-cost-and-goal-fn 50 50)]
    (doto canvas
      (.addMouseListener 
       (proxy [MouseAdapter] []
	 (mouseClicked 
	  [e]
	  (if (= (MouseEvent/BUTTON1) (.getButton e))
	    (let [sq-size (/ (min (.getHeight canvas) (.getWidth canvas)) grid-size)
		  x (int (/ (.getX e) sq-size))
		  y (int (/ (.getY e) sq-size))]
	      (swap! walls (fn [walls] 
			     (if (walls [x y])
			       (disj walls [x y])
			       (conj walls [x y]))))
	      (.repaint canvas)))))))
    (doto frame
      (.add canvas)
      (.setSize 600 600)
      (.setResizable true)
      (.setVisible true))))
    
	