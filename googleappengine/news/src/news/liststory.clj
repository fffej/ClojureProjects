(ns news.liststory                                    
  (:use (news appengine))
  (:gen-class :extends javax.servlet.http.HttpServlet))

(defn -doGet
  [_ request response]
  (.setContentType response "text/xml")
  (let [w (.getWriter response)]
    (.println w (str "{\"totalCount\": \"1\", \"stories\":[{\"body\": \"the quick brown fox\", \"title\": \"jumped over my code\"}]}"))))
	



