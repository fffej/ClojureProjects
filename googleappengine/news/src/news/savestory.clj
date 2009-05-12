(ns news.savestory                                    
  (:use (news appengine))
  (:gen-class :extends javax.servlet.http.HttpServlet)
  (:import (com.google.appengine.api.datastore DatastoreServiceFactory Entity Key Query)))
                                      
(defn store
  [data type]
  (let [entity (Entity. type)]
    (doseq [[k v] data]
      (.setProperty entity k v))
    (.put (DatastoreServiceFactory/getDatastoreService) entity)
    (.getKey entity)))

(defn -doGet                                                  
  [_ request response]
  (ensure-authenticated request response) ;; TODO define a macro to avoid this repetition in every servlet!
  (let [body (.getParameter request "bodytext")
	title (.getParameter request "title")]    
    (store {:body body :title title} "story")))