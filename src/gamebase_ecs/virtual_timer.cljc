(ns gamebase-ecs.virtual-timer)

(defn mk-timer []
  {:running? false})

(defn- -current-realtime-millis []
  #?(:clj (System/currentTimeMillis)
     :cljs (.getTime (js/Date.))))

(defn run [timer virtual-time]
  {:running? true, :offset (- virtual-time (-current-realtime-millis))})

(defn stop [timer]
  {:running? false})

(defn running? [timer]
  (:running? timer))

(defn get-time [{:keys [running? offset]}]
  "Get current virtual time. Timer must be running."
  (assert running?)
  (+ (-current-realtime-millis) offset))
