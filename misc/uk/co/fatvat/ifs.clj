(ns uk.co.fatvat.ifs
  (:import [javax.swing JFrame JPanel JSpinner SpinnerNumberModel])
  (:import [java.awt Color Polygon])
  (:import [java.awt.image BufferedImage])
  (:import [javax.swing.event MouseInputAdapter])
  (:use clojure.contrib.def))

(defvar width 512 "Width of the rendering plane")
(defvar height 512 "Height of the rendering plane")
(defvar running (atom false) "Are we going?")

(defvar source (BufferedImage. (inc width) (inc height) BufferedImage/TYPE_INT_RGB)
  "The current state of the image")

(defvar animator (agent [0 0])
  "The agent responsible for animation, value is the point")

(defstruct transform :function :prob)

(defn magnitude [x y] (Math/sqrt (+ (* x x) (* y y))))

(defn theta [x y] (Math/atan (/ x y)))

(defn phi [x y] (Math/atan (/ y x)))

(defn affine-transform
  "Perform an affine transform"
  [[a b c d e f] [x y]]
  [(+ (* a x) (* b y) e)
   (+ (* c x) (* d y) f)])

(defn sinusoidal
  [[a b c d e f] [x y]]
  (affine-transform (Math/sin x) (Math/sin y)))

(defn spherical
  [[a b c d e f] [x y]]
  (let [mag (/ 1 (+ (* x x) (* y y)))]
    (affine-transform (* mag x) (* mag y))))

(defn mk-linear-transform
  [[a b c d e f] prob]
  (struct transform (partial affine-transform [a b c d e f]) prob))

(defvar fern-leaf-transform
  (list
   (mk-linear-transform [0.85 0.04 -0.04 0.85 0.0 1.6] 0.85)
   (mk-linear-transform [0.2 -0.26 0.23 0.22 0 1.6] 0.92)
   (mk-linear-transform [-0.15 0.28 0.26 0.24 0 0.04] 0.99)
   (mk-linear-transform [0 0 0 0.16 0 0] 1.00))
  "List of attractors for the fern leaf [-2.1818 <= x <= 2.6556 0 <= y <= 9.95851]")

(defvar sierpinski-transform
  (list
   (mk-linear-transform [0.5 0.0 0 0.5 0 0] 0.33)
   (mk-linear-transform [0.5 0.0 0 0.5 0.25 (* 0.5 (/ (Math/sqrt 3) 2))] 0.66)
   (mk-linear-transform [0.5 0.0 0 0.5 0 0.5 0 ] 1.00))
  "The sierpinski transform")

(defvar random-transform
  (list
   (mk-linear-transform [0.5 0.0 0 0.5 0 0] 0.4)
   (mk-linear-transform [0.5 0.0 0 0.5 0.25 (* 0.5 (/ (Math/sqrt 3) 2))] 0.8)
   (mk-linear-transform [0.5 0.0 0 0.5 0 0.5 0 ] 1.00)))
   
(defn calculate-point
  "Calculate the next point to render based on the previous"
  [transform [x y]]
  (let [r (rand)
        f (:function (first (filter (fn [x] (< r (:prob x))) transform)))]
    (f [x y])))

(defn get-bounds
  [transform]
  (let [points (take 10000 (iterate (partial calculate-point transform) [0 0]))
        minx (apply min (map first points))
        miny (apply min (map second points))
        maxx (apply max (map first points))
        maxy (apply max (map second points))]
    [minx maxx miny maxy]))

(defn scale 
  "Scale x and y to be within the given bounds"
  [[x y] [minx maxx miny maxy]]
  (let [xscale (/ width (- maxx minx))
        yscale (/ height (- maxy miny))]
    [(Math/abs (dec (Math/floor (* (- x minx) xscale))))
     (Math/abs (dec (Math/floor (* (- y miny) yscale))))]))

(defn draw-ifs 
  "Draw the IFS, by kicking off an infinite drawing loop"
  [[x y] transform panel]
  (let [bounds (get-bounds transform)]
    (doseq [[px py] 
            (take-while
             (fn [_] @running)
             (iterate (partial calculate-point transform) [0 0]))]
      (let [[sx sy] (scale [px py] bounds)]
        (when (and (>= sx 0) (<= sx width) (>= sy 0) (<= sy height))
          (.setRGB source sx sy (rand Integer/MAX_VALUE)))
        (.repaint panel)))))

(defn launch-ui
  "Start up the UI"
  [transform]
  (let [frame (JFrame. "Iterated Function Systems (http://www.fatvat.co.uk/")
        panel (proxy [JPanel] []
                (paintComponent 
                 [g]
                 (proxy-super paintComponent g)
                 (.drawImage g source 0 0 nil)))]
    (doto frame
      (.addMouseListener (proxy [MouseInputAdapter] []
                           (mouseClicked 
                            [e]
                            (if @running
                              (swap! running (constantly false))
                              (do
                                (swap! running (constantly true))
                                (send-off animator draw-ifs transform panel))))))
      (.add panel)
      (.setSize width height)
      (.setVisible true))))
