(ns uk.co.fatvat.flock
  (:use clojure.contrib.def)
  (:import [javax.swing JFrame JTextField JLabel JPanel BoxLayout])
  (:import [java.awt Color]))

(defvar width 600 "Fixed height of simulation")
(defvar height 600 "Fixed width of simulation")
(defvar running (atom true) "Are we still going?")
(defvar agent-count 50 "Number of simultaneous objects")
(defvar max-velocity 9 "Maximum speed (in pixels/seconds)")
(defvar separation-distance 20 "Boids want to keep this far apart")

(defstruct boid :x :y :dx :dy :flock)

(defn mk-boid
  "Create a boid with the given parameters"
  [x y dx dy]
  (struct boid x y dx dy 0))

(defn magnitude
  [[x y]]
  (Math/sqrt (+ (* x x) (* y y))))

(defn velocity-change 
  "Alter the velocity dependent on the position"
  [pos min max]
  (if (< pos min) 10 (if (> pos max) -10 0)))

(defn bound-velocity
  "Bound the magnitude of the velocity vector"
  [[x y]]
  (let [m (magnitude [x y])]
    (if (> m max-velocity)
      [(* max-velocity (/ x m)) (* max-velocity (/ y m))]
      [x y])))

(defn update-position
  "Increment the position of the boid based on the current velocity.
   Position is bounded to be within width height range by altering velocity"
  [b]
  (let [newx (+ (:x b) (:dx b))
        newy (+ (:y b) (:dy b))]
    (mk-boid 
     newx
     newy
     (+ (:dx b) (velocity-change newx 0 width))
     (+ (:dy b) (velocity-change newy 0 height)))))

(defn alter-velocity
  "Mixin the new velocity"
  [b [vx vy]]
  (let [[newx newy] (bound-velocity [(+ vx (:dx b)) (+ vy (:dy b))])]
    (mk-boid
     (:x b)
     (:y b)
     newx
     newy)))

(defvar boids 
  (take agent-count
        (repeatedly (fn [] (agent (mk-boid 
                                   (rand-int width) 
                                   (rand-int height) 
                                   0
                                   0)))))
  "The boids in the system")

(defvar animator (agent nil)
  "The agent responsible for animation")

(defvar animation-delay-ms 100
  "The delay for running animations")

(defn distance
  "Distance between two points"
  [[x1 y1] [x2 y2]]
  (let [xd (- x2 x1)
        yd (- y2 y1)]
    (Math/sqrt (+ (* xd xd) (* yd yd)))))

(defn separation 
  "Avoid crowding neighbours (short-range repulsion)"
  [boid boids]
  (reduce 
   (fn [[x y] b] 
     [(- x (- (:x b) (:x boid)))
      (- y (- (:y b) (:y boid)))])
   [0 0]
   (filter
    (fn [b] (< (distance [(:x boid) (:y boid)] [(:x b) (:y b)]) separation-distance))
    boids)))

(defn alignment
  "Steer towards average heading of neighbours"
  [boid boids]
  (let [boid-count (count boids)
        [vx vy] (reduce (fn [[x y] b] [(+ x (:dx b)) (+ y (:dy b))])
                              [0 0]
                              boids)
        [avg-x avg-y] [(/ vx boid-count) (/ vy boid-count)]]
    [(double (/ avg-x 8)) (double (/ avg-y 8))]))

(defn cohesion
  "Steer towards average position of neighbours"
  [boid boids]
  (let [boid-count (count boids)
        [sx sy] (reduce (fn [[x y] b] 
                          [(+ x (:x b)) (+ y (:y b))])
                        [0 0]
                        boids)
        [mx my] [(int (/ sx boid-count)) (int (/ sy boid-count))]]
    [(int (/ (- mx (:x boid)) 100)) (int (/ (- my (:y boid)) 100))]))

(defn behave
  "Get the boid behaviour given boids"
  [boid boids]
  (let [[v1x v1y] (cohesion boid boids)
        [v2x v2y] (alignment boid boids)
        [v3x v3y] (separation boid boids)]
    (update-position 
     (alter-velocity
      (alter-velocity 
       (alter-velocity boid [v1x v1y])
       [v2x v2y])
      [v3x v3y]))))

(defvar canvas (proxy [JPanel] []
                 (paintComponent [g]
                   (proxy-super paintComponent g)
                   (.setColor g Color/RED)
                   (doseq [boid boids]
                     (.fillRect g (:x @boid) (:y @boid) 10 10))))
  "The rendering for all of the flock takes place here.")

(defn animate
  [x]
  (when @running
    (send-off *agent* animate) ;; *agent* means the agent that called this?
    (doseq [agent-boid boids]
      (send-off agent-boid behave (remove (partial = @agent-boid) (map deref boids))))
    (.repaint canvas)
    (Thread/sleep animation-delay-ms)))

(defn start-animate []
  (swap! running (constantly true))
  (send-off animator animate))
  
(defn stop-animate []
  (swap! running (constantly false)))
   
(defn flocking-ui []
  (let [frame (JFrame. "Demo of Flocking (http://www.fatvat.co.uk/)")]
    (doto frame
      (.add canvas)
      (.setSize width height)
      (.setVisible true))))