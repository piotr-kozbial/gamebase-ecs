(ns gamebase-ecs.core1
  (:require [clojure.spec.alpha :as s]
            [gamebase-ecs.event-queue :as eq]))

(defn anything? [x] true)

(s/def ::kind #{:world :system :component :entity})

(s/def ::system-key keyword?)
(s/def ::component-key keyword?)
(s/def ::entity-key (s/or :keyword keyword? :number number?))
(s/def ::entity-type keyword?)

;;;;; Object id's

(s/def ::world-id (s/keys :req [::kind]))
(s/def ::system-id (s/keys :req [::kind ::system-key]))
(s/def ::entity-id (s/keys :req [::kind ::entity-key]))
(s/def ::component-id (s/keys :req [::kind ::entity-key ::component-key]))
(defmulti id-spec ::kind)
(defmethod id-spec :world [_] ::world-id)
(defmethod id-spec :system [_] ::system-id)
(defmethod id-spec :entity [_] ::entity-id)
(defmethod id-spec :component [_] ::component-id)
(s/def ::id (s/multi-spec id-spec ::kind))

;; NOTE. The `id?` predicate is very simple, as it only checks for the presence of the ::kind attribute.
;; We assume that if the API is properly used and no external code manually creates maps with
;; keys in our namespace, then that check is sufficient to ensure that we have an object or id created by us.
;; If a user wants a full check, they can use spec directly: (s/valid? ::id x) or (s/valid? ::object x).
(defn id? [x]
  (boolean (::kind x)))
(s/fdef id?
  :args (s/cat :x anything?)
  :ret boolean?)

;;;;; Objects
;;
;; NOTE. Each object is its own id, but not vice-versa.

(s/def ::world (s/keys :req [::kind]))
(s/def ::system (s/keys :req [::kind ::system-key]))
(s/def ::entity (s/keys :req [::kind ::entity-key ::entity-type]))
(s/def ::component (s/keys :req
                           [::kind ::entity-key ::component-key ::system-key ::component-type]))
(defmulti object-spec ::kind)
(defmethod object-spec :world [_] ::world)
(defmethod object-spec :system [_] ::system)
(defmethod object-spec :entity [_] ::entity)
(defmethod object-spec :component [_] ::component)
(s/def ::object (s/multi-spec id-spec ::kind))

(defn to-id [object]
  (if (= (::kind object) :component)
    (select-keys object [::kind ::entity-key ::component-key])
    (select-keys object [::kind ::system-key ::entity-key ::component-key])))
(s/fdef to-id
  :args (s/cat :object ::id)
  :ret ::id)

;;; World

(defn world []
  {::kind :world
   ::entities {}
   ::systems {}
   ::time 0
   ::event-queue (eq/create)})
(s/fdef world
  :args (s/cat)
  :ret (s/and ::world ::world-id ::id))

(defn world-id []
  {::kind :world})
(s/fdef world-id
  :args (s/cat)
  :ret (s/and ::world-id ::id))

(defn world-id? [x]
  (= (::kind x) :world))
(s/fdef world-id?
  :args (s/cat :x anything?)
  :ret (s/and boolean?))

;;; System

(defn system [key]
  {::kind :system
   ::system-key key})
(s/fdef system
  :args (s/cat :key ::system-key)
  :ret (s/and ::system ::system-id ::id))

(defn system-key [x]
  ;; x can be:
  ;; - system key itself (a keyword)
  ;; - a system id/object
  ;; - a component object
  (if (keyword? x) x (::system-key x)))
(s/fdef system-key
  :args (s/cat :x (s/or :key ::system-key
                        :system (s/or :system ::system, :id ::system-id)
                        :component ::component))
  :ret ::system-key)

(defn system-id [x]
  ;; x can be:
  ;; - system key
  ;; - a system id/object
  ;; - a component object
  (if (keyword? x)
    {::kind :system
     ::system-key x}
    (if (= (::kind x) :system)
      (to-id x)
      {::kind :system
       ::system-key (::system-key x)})))
(s/fdef system-id
  :args (s/cat :x (s/or :key ::system-key
                        :system (s/or :system ::system, :id ::system-id)
                        :component ::component))
  :ret (s/and ::system-id ::id))

(defn system-id? [x]
  (= (::kind x) :system))
(s/fdef system-id?
  :args (s/cat :x anything?)
  :ret boolean?)

;;; Entity

(defn entity [key type]
  {::kind :entity
   ::entity-type type
   ::entity-key key
   ::components {}})
(s/fdef entity
  :args (s/cat :key ::entity-key, :type ::entity-type)
  :ret ::entity)

(defn entity-id [x]
  ;; x can be:
  ;; - entity key
  ;; - an entity id/object
  ;; - a component id/object
  (if (keyword? x)
    {::kind :entity
     ::entity-key x}
    (if (= (::kind x) :entity)
      (to-id x)
      {::kind :entity
       ::entity-key (::entity-key x)})))
(s/fdef entity-id
  :args (s/cat :x (s/or :key ::entity-key
                        :entity ::entity
                        :entity-id ::entity-id
                        :component ::component
                        :component-id ::component-id))
  :ret ::entity-id)

(defn entity-id? [x]
  (= (::kind x) :entity))
(s/fdef entity-id?
  :args (s/cat :x anything?)
  :ret boolean?)

(defn entity-key [x]
  ;; x can be:
  ;; - entity key itself (a keyword)
  ;; - an entity id/object
  ;; - a component id/object
  (if (keyword? x) x (::entity-key x)))
(s/fdef entity-key
  :args (s/cat :x (s/or :key ::entity-key
                        :entity ::entity
                        :entity-id ::entity-id
                        :component ::component
                        :component-id ::component-id))
  :ret ::entity-key)

;;; Component

(defn component [entity key system type]
  {::kind :component
   ::entity-key (entity-key entity)
   ::system-key (system-key system)
   ::type type
   ::component-key key})

(defn component-id [entity key]
  {::kind :component
   ::entity-key (entity-key entity)
   ::component-key key})

(defn component? [x]
  (= (::kind x) :component))
