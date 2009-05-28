(ns uk.co.fatvat.mlperceptron
  (:use [uk.co.fatvat.utils]))

(def bias 0)
(def activation-function (fn [x] (Math/tanh x)))
(def num-hidden 10)
(def learning-rate 0.05)

(defstruct network :hidden :output)

(defn create-network 
  [in-count hidden-count]
  (struct network (repeat hidden-count (take in-count (repeatedly rand))) (take hidden-count (repeatedly rand))))

(defn- run-hidden-layer
  [input network]
  (map * (get network :output)
       (map 
	(fn [n i] (activation-function (reduce + (map (partial * i) n))))
	(get network :hidden)
	input)))
  
(defn run-network
  [input network]
  (reduce + (run-hidden-layer input network)))

(defn update-hidden
  [input network hidden error]
  (map
   (fn [h o i] 
     (- h (* i h learning-rate error o (- 1 (* h h)))))
   hidden
   (get network :output)
   input))

(defn update-network
  [sample expected net]
  (let [hidden (run-hidden-layer sample net)
	error (- expected (reduce + hidden))
	updated-output (map (fn [h w] (- w (* learning-rate error h))) hidden (get net :output))] ;; TODO regularisation?
    (struct network
	    (update-hidden sample net hidden error)
	    updated-output)))
  
(defn train-network
  ([samples expected network]
     (if (empty? samples)
       network
       (recur (rest samples) (rest expected) (update-network (first samples) (first expected) network))))
  ([samples expected]
     (let [in-size (count (first samples))]
       (train-network samples expected (create-network in-size num-hidden)))))

