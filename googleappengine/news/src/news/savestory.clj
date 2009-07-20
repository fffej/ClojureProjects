(ns news.savestory  
  (:import [com.google.wave.api RobotMessageBundle])
  (:gen-class :extends com.google.wave.api.AbstractRobotServlet))
                                      
(defn -processEvents                                                  
  [_ bundle]
  (let [wavelet (.getWavelet bundle)]
    (when (.wasSelfAdded bundle)
      (let [blip (.appendBlip wavelet)
            textview (.getDocument blip)]
        (.append textview "Alive and Lispy")))
    (filter (fn [e] (= (.getType e) (EventType/WAVELET_PARTICIPANTS_CHANGED))) (.getEvents bundle))

