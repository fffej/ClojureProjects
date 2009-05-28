(ns uk.co.fatvat.utils)

(defn jiggle [data]
  (map (fn [x] (+ x (- (rand 0.05) 0.025))) data))

(def linearly-separable-test-data
     [(concat
       (take 100 (repeatedly #(jiggle [0 1 0])))
       (take 100 (repeatedly #(jiggle [1 0 0]))))
      (concat
       (repeat 100 0)
       (repeat 100 1))])

(def xor-test-data
     [(concat
       (take 100 (repeatedly #(jiggle [0 1])))
       (take 100 (repeatedly #(jiggle [1 0])))
       (take 100 (repeatedly #(jiggle [0 0])))
       (take 100 (repeatedly #(jiggle [1 1]))))
      (concat
       (repeat 100 1)
       (repeat 100 1)
       (repeat 100 0)
       (repeat 100 0))])
