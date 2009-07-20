(ns uk.co.fatvat.wave.parrot
  (:import [com.google.wave.api RobotMessageBundle EventType])
  (:gen-class :extends com.google.wave.api.AbstractRobotServlet))
                                      
(defn- add-blip
  [wavelet message]
  (.append (.getDocument (.appendBlip wavelet)) message))

(defn -processEvents                                                  
  [_ bundle]
  (let [wavelet (.getWavelet bundle)]
    (when (.wasSelfAdded bundle)
      (add-blip wavelet "Greetings.  I'm alive!"))
    (let [participant-changed-events (filter 
                                       (fn [e] (= (.getType e) (EventType/WAVELET_PARTICIPANTS_CHANGED))) 
                                       (.getEvents bundle))]
      (doseq [event participant-changed-events]
        (add-blip wavelet "Hey! It's me!")))))

