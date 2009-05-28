;;; jeff.foster@acm.org
;;; Single Layer Perceptron network
(ns uk.co.fatvat.perceptron)

(defn create-network 
  [in]
  (repeat in 0))

(defn run-network
  [input weights]
  (if (pos? (reduce + (map * input weights))) 1 0))

(def learning-rate 0.05)

(defn- update-weights
  [weights inputs error]
  (map 
   (fn [weight input] (+ weight (* learning-rate error input)))
   weights inputs))

(defn train
  ([samples expecteds] (train samples expecteds (create-network (count (first samples)))))
  ([samples expecteds weights]
     (if (empty? samples)
       weights
       (let [sample (first samples)
	     expected (first expecteds)
	     actual (run-network sample weights)
	     error (- expected actual)]
	 (recur (rest samples) (rest expecteds) (update-weights weights sample error))))))
