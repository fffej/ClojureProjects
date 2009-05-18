(ns uk.co.fatvat.robot.FatRobot
  (:gen-class :extends robocode.Robot))

(defn -run
  [robot]
  "Infinite loop whilst robot is alive"
  (.ahead robot 10)
  (.back robot 10)
  (recur robot))

(defn -onScannedRobotEvent
  [robot event]
  (doto robot
    (.fire 1)))