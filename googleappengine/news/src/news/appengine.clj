(ns news.appengine
  (:import (javax.servlet.http HttpServletRequest HttpServletResponse))
  (:import (com.google.appengine.api.users User UserService UserServiceFactory)))

(defn ensure-authenticated
  [request response]
  (let [userService (UserServiceFactory/getUserService)
	user (.getCurrentUser userService)]
    (if (nil? user) 
      (.sendRedirect response (.createLoginURL userService (.getRequestURI request)))
      true)))