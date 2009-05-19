(ns uk.co.fatvat.robot.FatRobot
  (:gen-class :extends robocode.Robot))

(defn -run
  [robot]
  "Infinite loop whilst robot is alive"
  (doto robot
    (.ahead 500)
    (.turnGunRight 360)
    (.back  500))
  (recur robot))

(defn -onScannedRobot
  [robot event]
  (doto robot
    (.fire 1)))