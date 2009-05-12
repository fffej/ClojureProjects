(ns news.savestory                                    
  (:use (news appengine))
  (:gen-class :extends javax.servlet.http.HttpServlet)
  (:import (com.google.appengine.api.users User UserService UserServiceFactory))
  (:import (javax.jdo JDOHelper PersistenceManager PersistenceManagerFactory)))
                                      
(defn -doGet                                                  
  [_ request response]
  (ensure-authenticated request response) ;; TODO define a macro to avoid this repetition in every servlet!
  (let [body (.getParameter request "bodytext")
	title (.getParameter request "title")]    
    ;; TODO persist this
    ))