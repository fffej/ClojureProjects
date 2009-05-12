(ns news.logout                                                                      
  (:use (news appengine))
  (:gen-class :extends javax.servlet.http.HttpServlet)
  (:import (com.google.appengine.api.users User UserService UserServiceFactory)))         
                                                                                                                   
(defn -doGet                                                  
  [_ request response]
  (ensure-authenticated request response) ;; TODO define a macro to avoid this repetition in every servlet!
  (let [userService (UserServiceFactory/getUserService)
	user (.getCurrentUser userService)]
    (when (not (nil? user))
      (.createLogoutURL userService (.getRequestURI request)))))
