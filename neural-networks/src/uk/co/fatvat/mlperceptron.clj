(ns uk.co.fatvat.mlperceptron
  (:use [uk.co.fatvat.utils]))

(def activation-function (fn [x] (Math/tanh x)))
(def activation-function-derivation (fn [y] (- 1.0 (* y y))))
(def num-hidden 10)

(def learning-rate 0.5)
(def momentum 0.1)

(defstruct bp-nn :weight-input :weight-output :change-input :change-output)

(defn make-matrix
  [width height]
  "Create a matrix (list of lists)"
  (repeat width (repeat height 0)))

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
  (let [i (inc input)] ;; bias term
    (struct bp-nn
	    (matrix-map (make-matrix i hidden) (fn [_] 0.2))      ;; TODO random range
	    (matrix-map (make-matrix hidden output) (fn [_] 2.0)) ;; TODO random range
	    (make-matrix i hidden)
	    (make-matrix hidden output))))

(defn apply-activation-function
  [wi pattern]
  "Calculate the hidden activations"
  (apply map (comp activation-function +) (map (fn [col p] (map (fn [row] (* row p)) col)) wi pattern)))

(defn calculate-hidden-deltas
  [wo ah od]
  "Calculate the error terms for the hidden"
  (let [error (map (fn [col] (reduce + (map (fn [row o] (* row o)) col od))) wo)]
    (map (fn [h e] (* e (activation-function-derivation h))) ah error)))

(defn update-weights
  [wo output-deltas co ah]
  (let [x (map 
	   (fn [wcol ccol h] 
	     (map (fn [wrow crow od] 
		    (let [change (* od h)]
		      [(+ wrow (* learning-rate change) (* momentum crow)) change]))
		  wcol ccol output-deltas))
	   wo co ah)]
    [(matrix-map x first) (matrix-map x second)]))

(defn run-network
  [pattern network]
  "Run the network with the given pattern and return the output and the hidden values"
  (assert (= (count pattern) (dec (count (get network :weight-input)))))
  (let [p (conj pattern 1)] ;; ensure bias term added
    (let [wi (get network :weight-input)
	  wo (get network :weight-output)
	  ah (apply-activation-function wi p)
	  ao (apply-activation-function wo ah)]
      [ao ah])))

(defn back-propagate
  [target pattern results network]
  "Back propagate the results to adjust the rates"
  (assert (= (count target) (count (first (get network :weight-output)))))
  (let [ao (first results)
	ah (second results)
	error (map - target ao)
	output-deltas (map (fn [o e] (* e (activation-function-derivation o))) ao error)
	hidden-deltas (calculate-hidden-deltas (get network :weight-output) ah output-deltas)
	wi (get network :weight-input)
	wo (get network :weight-output)
	ci (get network :change-input)
	co (get network :change-output)
	updated-output-weights (update-weights wo output-deltas co ah)
	updated-input-weights (update-weights wi hidden-deltas ci pattern)]
    (println updated-input-weights)
    (println updated-output-weights)
    (struct bp-nn
	    (first  updated-input-weights)
	    (first  updated-output-weights)
	    (second updated-input-weights)
	    (second updated-output-weights))
  ))

(comment 
(defn example[]
  (let [x (apply train-network (conj xor-test-data 10))]
    (println (first (run-network [0 0] x)) "-->" 0)
    (println (first (run-network [0 1] x)) "-->" 1)
    (println (first (run-network [1 0] x)) "-->" 1)
    (println (first (run-network [1 1] x)) "-->" 0))))
			     