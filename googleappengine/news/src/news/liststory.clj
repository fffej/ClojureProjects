(ns news.liststory                                    
  (:use (news appengine))
  (:gen-class :extends javax.servlet.http.HttpServlet))

(defn story-to-json
  [s]
  (str "{\"title\":\"" (get s "title") "\"" ",\"body\":" "\"" (get s "body") "\"},"))

(defn -doGet
  [_ request response]
  (.setContentType response "text/xml")
  (let [w (.getWriter response) stories (get-all "story")]
    (.println w (str "{\"totalCount\":" (count stories) ",\"stories\":["))
    (doseq [story stories]
      (.println w (str \tab (story-to-json story))))
    (.println w (str "]}"))))

	



