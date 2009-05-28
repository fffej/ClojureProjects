(ns uk.co.fatvat.mlperceptron)

(def bias 0)
(def learning-rate 0.05)
(def activation-function (fn [x] (Math/tanh x)))
(def num-hidden 10)

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

(defn update-network
  [sample expected network]
  (let [error (- expected (run-network sample network))]
    (struct network
	    (get :hidden network)
	    (map (fn [w] (- w (* learning-rate error))) (get :output network)))))
  

(defn train-network
  ([samples expected network]
     (if (empty? samples)
       network
       (recur (rest samples) (rest expected) (update-network (first samples) (first expected) network))))
  ([samples expected]
     (let [in-size (first samples)]
       (train-network samples expected (create-network in-size num-hidden)))))