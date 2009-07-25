(ns uk.co.fatvat.flock
  (:use clojure.contrib.def)
  (:import [javax.swing JFrame JTextField JLabel JPanel BoxLayout])
  (:import [java.awt Color])
  (:import [java.awt GridLayout]))


(defvar running (atom true) "Are we still going?")

(defvar agent-count 10 "Number of simultaneous objects")

(defstruct boid :x :y :direction :speed :flock)

(defn separation 
  "Avoid crowding neighbours (short-range repulsion"
  [])

(defn alignment
  "Steer towards average heading of neighbours"
  [])

(defn cohesion
  "Steer towards average position of neighbours"
  [])

(defvar boids (ref #{[10 10] [50 50] [80 80]})
  "All of the boids within the system")

(defvar canvas (proxy [JPanel] []
                 (paintComponent [g]
                   (proxy-super paintComponent g)
                   (println "Drawing...")
                   (.setColor g Color/RED)
                   (doseq [boid @boids]
                     (.fillRect g (first boid) (second boid) 10 10))))
  "The rendering for all of the flock takes place here.")

(defvar animator (agent nil)
  "The agent responsible for animation")

(defn animate
  [x]
  (when @running
    (send-off *agent* animate)
    (.repaint canvas)
    (Thread/sleep 1000)))

(defn start-animate []
  (swap! running (constantly true))
  (send-off animator animate))
  
(defn stop-animate []
  (swap! running (constantly false)))
   
(defn flocking-ui []
  (let [frame (JFrame. "Demo of Flocking (http://www.fatvat.co.uk/)")]
    (doto frame
      (.add canvas)
      (.setSize 300 300)
      (.setVisible true))))