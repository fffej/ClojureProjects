(ns news.appengine
  (:import (javax.servlet.http HttpServletRequest HttpServletResponse))
  (:import (com.google.appengine.api.users User UserService UserServiceFactory))
  (:import (com.google.appengine.api.datastore KeyFactory DatastoreServiceFactory Entity Key Query)))

(defn entity-to-map 
  [entity]
  (into (hash-map) (.getProperties entity)))
                                      
(defn get-entity
  [id type]
  (let [k (KeyFactory/createKey (.toString type) (Long/valueOf id))]
    (entity-to-map
     (.get (DatastoreServiceFactory/getDatastoreService) k))))

(defn store
  [data type]
  (let [entity (Entity. (.toString type))]
    (doseq [[k v] data]
      (.setProperty entity (.toString k) v))
    (.put (DatastoreServiceFactory/getDatastoreService) entity)
    (.getKey entity)))

(defn get-all
  [type]
  (let [query (Query. (str type))
	service (DatastoreServiceFactory/getDatastoreService)]
    (map entity-to-map (.asIterable (.prepare service query)))))

(defn ensure-authenticated
  [request response]
  (let [userService (UserServiceFactory/getUserService)
	user (.getCurrentUser userService)]
    (if (nil? user) 
      (.sendRedirect response (.createLoginURL userService (.getRequestURI request)))
      true)))