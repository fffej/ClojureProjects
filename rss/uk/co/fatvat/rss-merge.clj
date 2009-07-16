(ns uk.co.fatvat.rss-merge
  (:require [clojure.contrib.zip-filter :as zf])
  (:require [clojure.contrib.zip-filter.xml :as xm])
  (:require [clojure.zip :as zip])
  (:import [java.text SimpleDateFormat ParsePosition])
  (:use [clojure.contrib.http.agent])
  (:use [clojure.contrib.lazy-xml]))

(defn get-entries-from-rss
  "Get a list of entries from an RSS feed"
  [src]
  (xm/xml-> (zip/xml-zip (parse-trim src)) :entry))

(defn get-date
  "Given an RSS entry, get the date"
  [entry]
  (let [d (xm/xml-> entry :published xm/text)]
    (println d)
    (first d)))

(defn join-all
  "Join the lists provided using f to select an element each time"
  [f & lists]
  (let [l (remove empty? lists)]
    (when-not (empty? l)
      (let [n (reduce f (map first l))
	    c (count (filter (partial = true) (map (fn [x] (= (first x) n)) l)))
	    r (map (fn [x] (if (= (first x) n) (rest x) x)) l)]
	(lazy-seq
	  (concat
	   (repeat c n)
	   (apply join-all (cons f r))))))))

(defn min-date
  "Find the minimum date from an RSS entry"
  ([x] x)
  ([x y]
     (println "dates" (get-date x) (get-date y))
     (let [df (SimpleDateFormat. "yyyy-MM-d'T'HH:mm:ss'Z'")
	   d1 (.parse df (get-date x) (ParsePosition. 0))
	   d2 (.parse df (get-date y) (ParsePosition. 0))]
       (println "comparing " d1 d2)
       (if (.after d1 d2)
	 x
	 y))))
       

(defn rss-merge
  [feed1 feed2]
  (join-all
   min-date
   (get-entries-from-rss feed1)
   (get-entries-from-rss feed2)))
      

