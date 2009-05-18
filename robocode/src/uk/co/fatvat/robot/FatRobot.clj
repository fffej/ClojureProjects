(ns uk.co.fatvat.robot.FatRobot
  (:gen-class :extends robocode.Robot))

(defn -run
  [robot]
  "Infinite loop whilst robot is alive"
  (doto robot
    (.ahead 50)
    (.back  50))
  (recur robot))

(defn -onScannedRobotEvent
  [robot event]
  (doto robot
    (.fire 1)))