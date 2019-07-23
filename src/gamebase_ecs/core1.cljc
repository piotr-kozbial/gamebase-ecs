(ns gamebase-ecs.core1
  (:require [gamebase-ecs.event-queue :as eq]))

(defn id? [x]
  (boolean (::kind x)))

(defn to-id [object]
  (select-keys object [::kind ::system-key ::entity-key ::component-key]))

;;; World

(defn world []
  {::kind :world
   ::entities {}
   ::systems {}
   ::time 0
   ::event-queue (eq/create)})

(defn world-id []
  {::kind :world})

(defn world-id? [x]
  (= (::kind x) :world))

;;; System

(defn system [key]
  {::kind :system
   ::system-key key})

(defn system-id? [x]
  (= (::kind x) :system))

(defn system-key [x]
  ;; x can be:
  ;; - system key itself (a keyword)
  ;; - a system id/object
  ;; - a component id/object
  (if (keyword? x) x (::system-key x)))

(defn system-id [x]
  ;; x can be:
  ;; - system key itself (a keyword)
  ;; - a system id/object
  ;; - a component id/object
  (if (keyword? x)
    {::kind :system
     ::system-key x}
    (if (= (::kind x) :system)
      (to-id x)
      {::kind :system
       ::system-key (::system-key x)}))

  )

(defn entity [key type]
  {::kind :entity
   ::type type
   ::entity-key key
   ::components {}})

(defn entity? [x]
  (= (::kind x) :entity))

(defn entity-id [key]
  {::kind :entity
   ::entity-key key})

(defn entity-key [x]) ;; TODO

(defn component [entity key system type]
  {::kind :component
   ::entity-key (entity-key entity)
   ::system-key (system-key system)
   ::type type
   ::component-key key})

(defn component? [x]
  (= (::kind x) :component))


