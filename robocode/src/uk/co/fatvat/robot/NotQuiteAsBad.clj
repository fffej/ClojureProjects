(ns uk.co.fatvat.robot.NotQuiteAsBad
  (import (java.awt Color))
  (:gen-class :extends robocode.Robot :init create-robot :state state))

(defstruct target-details :distance :bearing :energy :velocity)

(defn -create-robot
  []
  "Robot records a list of events and performs actions based on these observations"
  [[] (ref [])])

(defn- setup-robot
  [robot]
  "Ensure robot looks pretty and has independent movement for radar and gun"
  (doto robot
    (.setColors Color/RED Color/BLACK Color/RED)
    (.setAdjustRadarForGunTurn true)
    (.setAdjustGunForRobotTurn true)))

(defn- walk
  [robot]
  "Go for a random walk to try and find someone to hurt"
  (doto robot
    (.ahead (rand-int 100))
    (.turnRight (rand-int 360))
    (.turnRadarLeft (rand-int 360))
    (.back (rand-int 100))
    (.turnLeft (rand-int 360))))

(defn- attack
  [robot history]
  "Based on the accrued events, hurt robots"
  (doto robot
    (.fire 3)))
	       
(defn- process-events
  [robot]
  (let [state @(.state robot)]
    (if (empty? state)
      (walk robot)
      (attack robot state))))

(defn -run
  [robot]
  "Infinite loop whilst robot is alive"
  (setup-robot robot)
  (loop [x 1] ;; TODO is there a better idiom for an infinite loop?
    (process-events robot)
    (recur 1))
  (recur robot))

(defn -onScannedRobot
  [robot event]
  (let [distance (.getDistance event)
	name (.getName event)
	energy (.getEnergy event)
	velocity (.getVelocity event)
	bearing (.getBearing event)
	state (.state robot)]
    (dosync
     (alter (.state robot) (cons (struct target-details distance bearing energy velocity) @state)))))