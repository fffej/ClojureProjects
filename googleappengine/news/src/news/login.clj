(ns news.login                                                                        
  (:use (news appengine))
  (:gen-class :extends javax.servlet.http.HttpServlet)
  (:import (com.google.appengine.api.users User UserService UserServiceFactory)))         
                                      
(defn greet 
  [user response]
  (.setContentType response "text/plain")
  (let [w (.getWriter response)]
    (.println w (str "Hello, " (.getNickname user)))))
                                                                                 
(defn -doGet                                                  
  [_ request response]
  (let [userService (UserServiceFactory/getUserService)
	user (.getCurrentUser userService)]
    (when (ensure-authenticated request response)
      (greet user response))))
