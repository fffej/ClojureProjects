(ns news.savestory                                    
  (:use (news appengine))
  (:gen-class :extends javax.servlet.http.HttpServlet)
  (:import (com.google.appengine.api.datastore DatastoreServiceFactory Entity Key Query)))
                                      
(defn store
  [data type]
  (let [entity (Entity. type)]
    (doseq [[k v] data]
      (.setProperty entity (.toString k) v))
    (.put (DatastoreServiceFactory/getDatastoreService) entity)
    (.getKey entity)))

(defn -doGet                                                  
  [_ request response]
  (let [body (.getParameter request "storyLink")
	title (.getParameter request "storyTitle")]    
    (store {:body body :title title} "story")))