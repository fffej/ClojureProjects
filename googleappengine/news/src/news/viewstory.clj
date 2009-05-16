(ns news.viewstory                                    
  (:use (news appengine))
  (:gen-class :extends javax.servlet.http.HttpServlet))

(defn -doGet                                                  
  [_ request response]
  (let [id (.getParameter request "storyId")
	story (get-entity id "story")
	w (.getWriter response)]
    (doseq [[k v] story]
      (.println w (str k "=" v)))))
