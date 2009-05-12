(ns news.savestory                                    
  (:use (news appengine))
  (:gen-class :extends javax.servlet.http.HttpServlet)
  (:import (com.google.appengine.api.datastore DatastoreServiceFactory Entity Key Query)))
                                      
;; We store maps.  Each map has a globally unique ID
;; Data is immutable to make life easier

(defn- remove-key [m]
  (remove (fn [x] (= (first x) :key)) m))

(defn store
  [data type]
  (let [entity (Entity. type)]
    (doseq [[k v] data]
      (.setProperty entity k v))
    (.put (DatastoreServiceFactory/getDatastoreService) entity)))

(defn -doGet                                                  
  [_ request response]
  (ensure-authenticated request response) ;; TODO define a macro to avoid this repetition in every servlet!
  (let [body (.getParameter request "bodytext")
	title (.getParameter request "title")]    
    ;; TODO persist this
    ))