(ns uk.co.fatvat.mlperceptron
  (:use [uk.co.fatvat.utils]))

(def activation-function (fn [x] (Math/tanh x)))
(def activation-function-derivation (fn [y] (- 1.0 (* y y))))
(def num-hidden 10)

(def learning-rate 0.5)
(def momentum 0.1)

(defstruct bp-nn :weight-input :weight-output :momentum-input :momentum-output)

(defn make-matrix
  [width height]
  "Create a matrix (list of lists)"
  (repeat height (repeat width 0)))

(defn matrix-map
  [m func]
  "Apply a function to every element in a matrix"
  (map (fn [x] (map func x)) m))

(defn rand-range 
  [l h]
  "Return a real number within the given range"
  (+ (rand (- h l)) l))

(defn create-network
  [input hidden output]
  "Create a network with the given number of input, hidden and output nodes"
  (struct bp-nn
	  (matrix-map (make-matrix input hidden) (fn [_] (rand-range -0.2 0.2)))
	  (matrix-map (make-matrix hidden output) (fn [_] (rand-range -2.0 2.0)))
	  (make-matrix input hidden)
	  (make-matrix hidden output)))

(defn matrix-reduce 
  [matrix column f]
  "Multiply each row in the matrix by the column and apply f to the sum of the resulting elements"
  (map
   (fn [w p]
     (f (reduce + (map (partial * p) w))))
   matrix
   column))

(defn run-network
  [pattern network]
  "Run the network with the given pattern and return the output and the hidden values"
  (assert (= (count pattern) (count (first (get network :weight-input)))))
  (let [wi (get network :weight-input)
	wo (get network :weight-output)
	ah (matrix-reduce wi pattern activation-function)] 
    [(matrix-reduce wo ah activation-function) ah]))

(defn update-weights
  [m c p pd]
  "Update the weights for the given matrix, with the momentum, pattern and delta"
  (map (fn [w cx h d] 
	 (let [v (* h d)]
	   (map (fn [o c] [(+ o (* learning-rate v) (* momentum c)) v]) w cx)))
       m c p pd))

(defn back-propogate
  [pattern output expected hidden network]
  "Apply back propogation to the network with the given pattern and error"
  (assert (= (count expected) (count (get network :weight-output))))
  (let [co (get network :momentum-output)
	ci (get network :momentum-input)
	wo (get network :weight-output)
	wi (get network :weight-input)
	output-deltas (map (fn [o e] (* (activation-function-derivation o) (- e o))) output expected)
	hidden-deltas (map (fn [x y] (* (activation-function-derivation x) y)) output (matrix-reduce wo output-deltas identity))
	error  (reduce + (map (fn [e o] (let [d (- e o)] (* 0.5 (* d d)))) expected output))
        output-updates (update-weights wo co hidden output-deltas)
	input-updates  (update-weights wi ci pattern hidden-deltas)]
    [(struct bp-nn
	    (matrix-map input-updates first)
	    (matrix-map output-updates first)
	    (matrix-map input-updates second)
	    (matrix-map output-updates second)) error]))

(defn run-iteration
  [network patterns expecteds]
  (let [results (map 
		 (fn [pattern expected]
		   (let [output (run-network pattern network)]
		     (back-propogate pattern (first output) expected (second output) network)))
		 patterns expecteds)
	error (reduce + (map second results))]
    (println "Error this iteration: " error)
    (first (last results)))) ;; TODO grossly inefficient!
    
(defn train-network
  ([patterns expected iterations]
     (let [n (create-network (count (first patterns)) 10 (count (first expected)))]
       (train-network patterns expected n iterations)))
  ([patterns expected network iterations]
     (if (zero? iterations)
       network
       (recur patterns expected (run-iteration network patterns expected) (dec iterations)))))

(defn example[]
  (let [x (apply train-network-2 (conj xor-test-data 1000))]
    (println (first (run-network [0 0] x)) "-->" 0)
    (println (first (run-network [0 1] x)) "-->" 1)
    (println (first (run-network [1 0] x)) "-->" 1)
    (println (first (run-network [1 1] x)) "-->" 0)))
			     