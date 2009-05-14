(ns news.liststory                                    
  (:use (news appengine))
  (:gen-class :extends javax.servlet.http.HttpServlet))

(defn -doGet
  [_ request response]
  (.setContentType response "text/xml"))



