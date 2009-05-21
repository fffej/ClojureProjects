(ns uk.co.fatvat.robot.NotQuiteAsBad
  (import (java.awt Color))
  (import (robocode Rules))
  (import (robocode.util Utils))
  (:gen-class :extends robocode.AdvancedRobot :init create-robot :state state))

(defstruct target-details :distance :bearing :energy :velocity)

(defn -create-robot
  []
  "Robot records a list of events and performs actions based on these observations"
  [[] (ref [])])

(defn- setup-robot
  [robot]
  "Ensure robot looks pretty"
  (doto robot
    (.setAdjustRadarForGunTurn true)
    (.setColors Color/RED Color/BLACK Color/RED)))

(defn- attack
  [robot]
  "Based on the accrued events, hurt robots"
  (let [latest (last @(.state robot))]
    (.turnRight robot (get latest :bearing))
    (when (zero? (get latest :velocity))
      (.fire robot 3))
    (.setTurnRadarRight robot 360)))

(defn- walk
  [robot]
  "Go for a walk around the outside of the building"
  (let [x (mod (.getHeading robot) 90)]
    (.ahead robot 50)
    (when (not (zero? x))
      (.turnLeft robot x))
    (when (zero? (.getVelocity robot))
      (.turnRight robot 90))))

(defn -run
  [robot]
  "Infinite loop whilst robot is alive"
  (setup-robot robot)
  (loop [x 1] ;; TODO is there a better idiom for an infinite loop?
   (walk robot)
    (recur 1)))

(defn -onScannedRobot
  [robot event] 
  (let [distance (.getDistance event)
	name (.getName event)
	energy (.getEnergy event)
	velocity (.getVelocity event)
	bearing (.getBearing event)]
    (dosync
     (alter (.state robot) conj (struct target-details distance bearing energy velocity)))
    (attack robot)))
  