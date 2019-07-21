(ns gamebase-ecs.core
  (:require [gamebase-ecs.event-queue :as eq]))

;;;;;;;;;;;;;;;;;;;;;; P U B L I C ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *with-xprint* false)

;;;;; Generic schema utilities

;; (defn s-literal
;;   "schema for a single fixed value"
;;   [v]
;;   (s/pred #(= % v)))

;;;;; Event targets

;; (def sTarget
;;   "Schema for event targets (? then why the generic map?)"
;;   (s/conditional
;;    #(= (::kind %) :to-world)
;;    ,   {s/Any s/Any}
;;    #(= (::kind %) :to-system)
;;    ,   {::system-id s/Any
;;         s/Any s/Any}
;;    #(= (::kind %) :to-entity)
;;    ,   {::entity-id s/Any
;;         s/Any s/Any}
;;    #(= (::kind %) :to-component)
;;    ,   {::entity-id s/Any
;;         ::component-key s/Any
;;         s/Any s/Any}))

(defn to-world []
  {::kind :to-world})

(defn to-system [system]
  {::kind :to-system
   ::system-id (if (keyword? system)
                 system
                 (::system-id system))})

(defn to-entity [entity]
  {::kind :to-entity
   ::entity-id
   (if (map? entity)
     (::entity-id entity)
     entity)})

(defn to-component
  ([component]
   {::kind :to-component
    ::entity-id (::entity-id component)
    ::component-id (::component-key component)})
  ([entity-id component-id]
   {::kind :to-component
    ::entity-id entity-id
    ::component-id component-id}))

(defn to [object-or-target-id]
  (if (map? object-or-target-id)
    (case (::kind object-or-target-id)
      :world (to-world)
      :to-world object-or-target-id
      :system (to-system object-or-target-id)
      :to-system object-or-target-id
      :entity (to-entity object-or-target-id)
      :to-entity object-or-target-id
      :component (to-component object-or-target-id)
      :to-component object-or-target-id
      )
    object-or-target-id))


(defn change-target [event target-id]
  (assoc event ::target-id (to target-id)))

;;;;; Objects

;; (def sComponent
;;   {::kind (s-literal :component)
;;    ::type s/Any
;;    ::system-id s/Any
;;    ::entity-id s/Any
;;    ::component-key s/Any
;;    s/Any s/Any})

;; (def sEntity
;;   {::kind (s-literal :entity)
;;    ::type s/Any
;;    ::entity-id s/Any
;;    ::components {s/Any sComponent}
;;    s/Any s/Any})

;; (def sSystem
;;   {::kind (s-literal :system)
;;    ::system-id s/Any
;;    s/Any s/Any})

;; (def sWorld
;;   {::kind (s-literal :world)
;;    ::systems {s/Any sSystem}
;;    ::entities {s/Any sEntity}
;;    s/Any s/Any})

;; (def sObject
;;   (s/conditional
;;    #(= (::kind %) :world) sWorld
;;    #(= (::kind %) :system) sSystem
;;    #(= (::kind %) :entity) sEntity
;;    #(= (::kind %) :component) sComponent))

(defn id [object-or-object-id]
  (if (map? object-or-object-id)
    ;; if a map, then maybe an object
    (case (::kind object-or-object-id)
      :world nil
      :system (::system-id object-or-object-id)
      :component (::component-id object-or-object-id)
      :entity (::entity-id object-or-object-id)
      ;; if not an object, then this map is the id itself
      object-or-object-id)
    ;; if not map, then not object, then what we got was id itself
    object-or-object-id))

(def s-world
  ;; {::kind (s/pred #{:world})
  ;;  ::entities s/Any
  ;;  ::systems s/Any
  ;;  ::time s/Any
  ;;  ::event-queue s/Any}
  )

(defn mk-world []
  {::kind :world
   ::entities {}
   ::systems {}
   ::time 0
   ::event-queue (eq/create)})

(defn mk-system [id]
  {::kind :system
   ::system-id id})

(defn mk-entity [id type]
  {::kind :entity
   ::type type
   ::entity-id id
   ::components {}})

;; (defn add-component-to-entity [e c]
;;   (assoc-in e [::components (::entity-id e)] )

;;   )

(defn mk-component [system-or-id entity-or-id key type]
  (let [v {::kind :component
           ::system-id (id system-or-id)
           ::type type
           ::entity-id (id entity-or-id)
           ::component-key key}]
    (if *with-xprint*
      (with-meta v
        {:app.xprint.core/key-order [::kind ::system-id ::type ::entity-id ::component-key]})
      v)))

;;;;; Event handling

;; Return schema for `handle-event`. It may return
;; - an object: world, system, entity or component,
;; - a sequential of objects.
;; (def sCallHandleReturn (s/conditional
;;                         map? sObject
;;                         sequential? [sObject]))

;; Note. It is not possible to assign schema to a multimethod.
;; Schema will be controlled in the implementation
;; of do-handle-event.
;; This method (`handle-event`) is to be implemented by clients,
;; but not called directly.
;; Client should always call `do-handle-event`.
(defmulti handle-event
  (fn [world event, object]
    (let [target-id (::target-id event)]
      (case (::kind target-id)
        ;; these instances to be handled on a global (world) level
        :to-world
        ,   [:to-world (::msg event)]
        ;; these instances to be defined by given system
        :to-system
        ,   [:to-system (::system-id target-id) (::msg event)]
        ;; these instances to be defined next to entity constructor
        :to-entity
        ,   [:to-entity (::type object) (::msg event)]
        ;; these instances to be defined by system
        ;; to which component belongs
        :to-component
        ,   [:to-component (::type object) (::msg event)]

        (println (str "PROBLEM 2 !!!! >" (pr-str target-id) "< EVENT: " (pr-str event)))
        ))))

(defmethod handle-event :default
  [_ e _]
  (println (str "UNHANDLED EVENT: " (pr-str e)))
  ;; TODO - this ignores all unknown events
  ;; Maybe there should be a warning at least
  ;; or maybe we should examine the kind of event.
  ;; Maybe there should be a list of events that objects are allowed to ignore
  ;; (i.e. not define a method for it).
  [])


(declare resolve-target-id ;; private
         insert-object)    ;; private

;; Event-handling function to be called by clients.
;; Always returns world.
(defn do-handle-event [world event]

  (let [world0 (assoc world ::time (:gamebase.event-queue/time event))
        object (resolve-target-id world0 (::target-id event))
        ret (handle-event
             world0
             event object)
        new-objects-or-events
        (if (map? ret)
          [ret] ;; single object - pack into vector to make it seqable
          ret)

        world' (reduce
                insert-object
                world0
                (filter ::kind new-objects-or-events))

        events' (remove nil? (remove ::kind new-objects-or-events))

        event-queue' (eq/put-all-events (::event-queue world') events')

        world'' (assoc world' ::event-queue event-queue')]

    world''))


(defn advance-until-time [world time]
  (loop [wrl world]
    (if-let [s-t (eq/soonest-event-time (::event-queue wrl))]
      (if (<=  s-t time)
        (let [[event event-queue'] (eq/take-event (::event-queue wrl))
              wrl' (assoc wrl ::event-queue event-queue'
                          ::time s-t)]
          (recur (do-handle-event wrl' event)))
        wrl)
      (assoc wrl ::time time))))

;; TODO - event time should be already set here (to 0 by default)
;; (now it is only defined in event-queue, but that is inconvenient)
(defn mk-event [target-or-id msg time]
  (let [target-id (to target-or-id)]
    (when-not target-id
      (println "ERROR!!! TARGET-ID NIL!!!")
      (/ 1 0))
    (when-not (::kind target-id)
      (println (str "ERROR!!! TARGET-ID KIND NIL!!! " (pr-str target-id)))
      (/ 1 0))
    {::target-id target-id
     ::msg msg
     :gamebase.event-queue/time time}))

;;;;; Putting events into queue

(defn put-all-events [world events]
  (let [event-queue' (eq/put-all-events (::event-queue world) events)]
    (assoc world ::event-queue event-queue')))


;;;;; Predefined events

(def predefined-events

  ;; Just to notify that maybe some time has passed.
  ::time

  )

;;;;; Event handling helpers

(defn retarget [event object]
  (assoc event ::target-id (to object)))

(defn pass-event-through-all [world event objects]
  (reduce
   (fn [w o] (do-handle-event w (retarget event o)))
   world
   objects))

(defn all-systems [world]
  (vals (::systems world)))

(defn all-entities [world]
  (vals (::entities world)))

(defn all-components [world]
  (->> (vals (::entities world))
       (mapcat (comp vals ::components))))

(defn all-components-of-system [world system]
  (filter
   #(= (id system) (::system-id %))
   (all-components world)))


(defn get-entity [world component]
  (let [entity-id (::entity-id component)]
    ((::entities world) entity-id)))


(defn ck-kvs [component-key & kvs]
  (into [::components component-key] kvs))


;;;;;;;;;;;;;;;;;;;;;; p r i v a t e ;;;;;;;;;;;;;;;;;;;;;;;;;;
nil


;; helper function
(defn resolve-target-id [world target-id]
  (case (::kind target-id)
    :to-world
    ,  world
    :to-system
    ,  ((::systems world) (::system-id target-id))
    :to-entity
    ,  ((::entities world) (::entity-id target-id))
    :to-component
    ,  (let [entity ((::entities world) (::entity-id target-id))]
         ((::components entity) (::component-id target-id)))))

;; helper function
(defn insert-object [world object]
  (case (::kind object)
    :world
    ,  object ;; we replace the whole world
    :system
    ,  (assoc-in world [::systems (::system-id object)] object)
    :entity
    ,  (assoc-in world [::entities (::entity-id object)] object)
    :component
    ,  (assoc-in world [::entities (::entity-id object)
                        ::components (::component-key object)] object)))

;; helper function
(defn remove-entity-by-key [world entity-key]
  (update-in world [::entities] dissoc entity-key))

(defn get-entity-by-key [world entity-key]
  (get-in world [::entities entity-key]))

(defn get-entity-by [world entity-or-id]
  (if (map? entity-or-id)
    entity-or-id ;; TODO - should check more?
    (get-entity-by-key world entity-or-id)))
