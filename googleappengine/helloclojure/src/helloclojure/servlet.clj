(ns helloclojure.servlet
  (:gen-class :extends javax.servlet.http.HttpServlet))

(defn -doGet
  [_ request response]
  (let [w (.getWriter response)]
    (.println w "Hello world!")))
