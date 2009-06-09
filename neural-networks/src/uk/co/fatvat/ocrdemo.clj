(ns uk.co.fatvat.ocrdemo
  (:import (java.io File))
  (:import (java.awt Graphics2D RenderingHints))
  (:import (java.awt.image BufferedImage))
  (:import (javax.imageio ImageIO))
  (:use [uk.co.fatvat.mlperceptron]))

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

(defn load-image
  [path]
  "Read the image from the given path and normalizes it to pixels in the range [0..1]"
  (let [src (ImageIO/read (File. path))
	dest (BufferedImage. 16 24 BufferedImage/TYPE_INT_RGB)]
    (doto (.createGraphics dest)
      (.setRenderingHint RenderingHints/KEY_INTERPOLATION RenderingHints/VALUE_INTERPOLATION_BICUBIC)
      (.drawImage src 0 0 16 24 nil)
      (.dispose))
    dest))

(defn create-training-data
  [fonts])
  
