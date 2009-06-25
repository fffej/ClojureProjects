(ns uk.co.fatvat.debug
  (:use [clojure.set]))

(def *dbg-ids* (ref #{:gps}))

(defn dbg 
  "Print debugging info if (DEBUG id) has been specified"
  [id format-string & args]
  (when (contains? id @*dbg-ids*)
    (println (format format-string args))))

(defn debug
  "Start dbg output on the given ids"
  [& ids]
  (dosync
   (alter *dbg-ids* (fn [x] (set (union x ids))))))

(defn undebug
  "Stop dbg output on the given ids"
  [& ids]
  (dosync
   (alter *dbg-ids* (fn [x] (difference x ids)))))

(defn dbg-indent
  "Print indented debugging info if (DEBUG ID) has been specified"
  [id indent format-string & args]
  (when (@*dbg-ids* id)
    (println (format (str (apply str (repeat indent \space)) format-string) args))))

