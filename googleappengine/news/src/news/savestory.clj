(ns news.savestory                                    
  (:use (news appengine))
  (:gen-class :extends javax.servlet.http.HttpServlet))
                                      
(defn -doGet                                                  
  [_ request response]
  (let [body (.getParameter request "storyLink")
	title (.getParameter request "storyTitle")]    
    (let [w (.getWriter response)]
      (.println w (store {"body" body "title" title} "story")))))