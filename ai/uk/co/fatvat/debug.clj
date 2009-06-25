(ns uk.co.fatvat.debug
  (:use [clojure.set]))

(def *dbg-ids* (ref #{:gps}))

(defn dbg 
  [id format-string & args]
  "Print debugging info if (DEBUG id) has been specified"
  (when (contains? id @*dbg-ids*)
    (println (format format-string args))))

(defn debug
  [& ids]
  "Start dbg output on the given ids"
  (dosync
   (alter *dbg-ids* (fn [x] (set (union x ids))))))

(defn undebug
  [& ids]
  "Stop dbg output on the given ids"
  (dosync
   (alter *dbg-ids* (fn [x] (difference x ids)))))

(defn dbg-indent
  [id indent format-string & args]
  "Print indented debugging info if (DEBUG ID) has been specified"
  (when (@*dbg-ids* id)
    (println (format (str (apply str (repeat indent \space)) format-string) args))))

