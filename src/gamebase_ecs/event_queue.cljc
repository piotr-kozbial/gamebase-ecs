(ns gamebase-ecs.event-queue)

(defn create []
  {:set_ #{}
   :sq 0
   :n 0})

(defn put-event [q event]
  (when-not event
    (println "ALARM! event is nil!!!")
    (assert false))
  (when (= event '())
    (println "ALARM! event is ()!!!")
    (assert false))
  (when (nil? (::time event))
    (println "ALARM! ::time of event is nil!!!")
    (assert false))
  ;; (when-not (= (first event) :update)
  ;;   (println (str "[QUEUE] <- " (pr-str event))))
  (let [{:keys [set_ sq n]} q]
    (assoc q
           :set_ (conj set_ (assoc event :sq sq))
           :sq (inc sq)
           :n (inc n))))

(defn put-all-events [q events]
  (reduce put-event q events))

(defn take-event [q]
  (let [all (:set_ q)
        all-sorted (sort-by #(vector (::time %) (:priority %) (:sq %)) all)
        soonest-event (first all-sorted)
        rest-of-events (rest all-sorted)]
    ;; (when-not (= (first soonest-event) :update)
    ;;   (println (str "[QUEUE] => " (pr-str soonest-event))))
    [soonest-event
     (assoc q
            :set_ rest-of-events
            :n (dec (:n q)))]))

(defn soonest-event-time [q]
  (let [all (:set_ q)]
    (->> all
         (map ::time)
         (sort)
         (first))))

(defn take-events-until  [q time]
  (let [all (:set_ q)]
    [(->> all
          (filter #(<= (::time %) time))
          (sort-by #(vector (::time %) (:priority %) (:sq %))))
     (assoc q :set_ (remove #(<= (::time %) time) all))]))
