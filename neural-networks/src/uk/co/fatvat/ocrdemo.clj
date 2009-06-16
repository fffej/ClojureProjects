(ns uk.co.fatvat.ocrdemo
  (:import (java.io File))
  (:import (java.awt Graphics2D RenderingHints))
  (:import (java.awt.image BufferedImage))
  (:import (javax.imageio ImageIO))
  (:use [uk.co.fatvat.mlperceptron]))

(def image-width 8)

(def image-height 8)

(def base-directory "/home/jfoster/images/numbers/")

(def font-names #{
  "AvantGarde-Book"
  "AvantGarde-BookOblique"
  "AvantGarde-Demi"
  "AvantGarde-DemiOblique"
  "Bookman-Demi"
  "Bookman-DemiItalic"
  "Bookman-Light"
  "Bookman-LightItalic"
  "Courier"
  "Courier-Bold"
  "Courier-BoldOblique"
  "Courier-Oblique"
  "fixed"
  "Helvetica"
  "Helvetica-Bold"
  "Helvetica-BoldOblique"
  "Helvetica-Narrow"
  "Helvetica-Narrow-Bold"
  "Helvetica-Narrow-BoldOblique"
  "Helvetica-Narrow-Oblique"
  "Helvetica-Oblique"
  "NewCenturySchlbk-Bold"
  "NewCenturySchlbk-BoldItalic"
  "NewCenturySchlbk-Italic"
  "NewCenturySchlbk-Roman"
  "Palatino-Bold"
  "Palatino-BoldItalic"
  "Palatino-Italic"
  "Palatino-Roman"
  "Symbol"
  "Times-Bold"
  "Times-BoldItalic"
  "Times-Italic"
  "Times-Roman"
})

(defn normalize
  [coll]
  (let [minimum (apply min coll)  ;; inefficient should get both in one go!
	maximum (apply max coll)
	diff (- maximum minimum)]
    (map (fn [x] 
	   (/ (- x minimum) diff)) coll)))


(defn load-image
  [path]
  "Read the image from the given path and normalizes it to pixels in the range [0..1]"
  (let [src (ImageIO/read (File. path))]
    (normalize (into [] (.getRGB src 0 0 image-width image-height (int-array (* image-width image-height)) 0 image-width)))))

(defn create-training-data
  [fonts]
  [(mapcat 
    (fn [font] 
      (mapcat (fn [x] 
		(let [image (load-image (str base-directory font "_" x ".gif"))]
		  [image]))
	      (range 0 10)))
    (apply concat (repeat 10 fonts)))
   (mapcat
    (fn [_] (map (fn [x] (assoc (into [] (repeat 10 0)) x 1)) (range 0 10)))
    (apply concat (repeat 10 fonts)))])

(def ocr-training-data 
     (let [x (create-training-data (take 1 font-names))]
       x))

(defn ocr-example[]
  (let [x (nth (apply train-network ocr-training-data) 100)
	[samples expecteds] ocr-training-data]
    (doseq [num (range 0 10)]
      (println (first (run-network (nth (first ocr-training-data) num) x)) "-->" num))))
