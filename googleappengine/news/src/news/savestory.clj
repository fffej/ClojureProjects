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
  (let [body (.getParameter request "storyLink")
	title (.getParameter request "storyTitle")]    
    (.setContentType response "text/plain")
    (let [w (.getWriter response)]
      (.println w (str body title)))))
;;      (.println w (str "Hello, " (.getNickname user)))))


   ;; (store {:body body :title title} "story")))