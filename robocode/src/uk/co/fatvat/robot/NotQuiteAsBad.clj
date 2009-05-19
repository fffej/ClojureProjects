(ns uk.co.fatvat.robot.NotQuiteAsBad
  (import (java.awt Color))
  (:gen-class :extends robocode.Robot :init create-robot :state state))

(defstruct target-details :distance :bearing :energy :velocity)

(defn -create-robot
  []
  "Create a new robot and use it to record state"
  [[] (ref {})])

(defn- setup-robot
  [robot]
  "Ensure robot looks pretty and has independent movement for radar and gun"
  (doto robot
    (.setColors Color/RED Color/BLACK Color/RED)
    (.setAdjustRadarForGunTurn true)
    (.setAdjustGunForRobotTurn true)))

(defn -run
  [robot]
  "Infinite loop whilst robot is alive"
  (setup-robot robot)
  (loop [x 1] ;; TODO is there a better idiom for an infinite loop?
    (doto robot
      (.ahead 10)
      (.back 10)
      (.fire 3))
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
     (alter (.state robot)
	    assoc name (cons (struct target-details distance bearing energy velocity) (get @state name []))))))