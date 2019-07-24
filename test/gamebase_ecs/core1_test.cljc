(ns gamebase-ecs.core1-test
  (:require [gamebase-ecs.core1 :as sut]
            [gamebase-ecs.event-queue :as eq]
            #?(:clj [clojure.test :as t :refer [deftest testing is]]
               :cljs [cljs.test :as t :include-macros true])))

(def non-ids
  [(sut/id? 25)
   (sut/id? :world)
   (sut/id? {:kind :world})
   (sut/id? {:kind :system :system-key :x})
   (sut/id? {:kind :entity})
   (sut/id? {:kind :component})])

(deftest id-general
  (testing "Id? predicate"
    (testing "against object constructors"
      (is (sut/id? (sut/world)))
      (is (sut/id? (sut/system :abc)))
      (is (sut/id? (sut/entity :abc :type1)))
      (is (sut/id? (sut/component :ent :comp :sys :typ))))
    (testing "against id constructors"
      (is (sut/id? (sut/world-id)))
      (is (sut/id? (sut/system-id :abc)))
      (is (sut/id? (sut/entity-id :abc)))
      (is (sut/id? (sut/component-id :ent :comp))))
    (testing "against non-id's"
      (doseq [x non-ids]
        (is (not (sut/id? x))))))
  (testing "To-id conversion"
    (testing "from id constructors, against id constructors (idempotency)"
      (doseq [x [(sut/world-id)
                 (sut/system-id :xyz)
                 (sut/entity-id :xyz)
                 (sut/component-id :e :c)]]
        (is (= (sut/to-id x) x))))
    (testing "from object constructors, against id constructors"
      (is (= (sut/to-id (sut/world)) (sut/world-id)))
      (is (= (sut/to-id (sut/system :xyz)) (sut/system-id :xyz)))
      (is (= (sut/to-id (sut/entity :xyz :T)) (sut/entity-id :xyz)))
      (is (= (sut/to-id (sut/component :e :c :s :t)) (sut/component-id :e :c))))))

(deftest world
  (testing "Literal"
    (is (sut/world) {::kind :world
                     ::entities {}
                     ::systems {}
                     ::time 0
                     ::event-queue (eq/create)})
    (is (sut/world-id) {::kind :world}))
  (testing "World-id? predicate"
    (testing "against right constructors"
      (is (sut/world-id? (sut/world)))
      (is (sut/world-id? (sut/world-id))))
    (testing "against wrong constructors"
      (is (not (sut/world-id? (sut/system :xyz))))
      (is (not (sut/world-id? (sut/system-id :xyz))))
      (is (not (sut/world-id? (sut/entity :aaa :kkk))))
      (is (not (sut/world-id? (sut/entity-id :aaa))))
      (is (not (sut/world-id? (sut/component :aaa :kkk :sys :tp))))
      (is (not (sut/world-id? (sut/component-id :aaa :kkk)))))
    (testing "against non-ids"
      (doseq [x non-ids]
        (is (not (sut/world-id? x)))))))

(deftest system
  (testing "Literal"
    (let [system-id (sut/system-id :foo)
          system (sut/system :foo)
          component (sut/component :e :c :sys :typ)]
      (is (sut/system :foo)      {::kind :system ::system-key :foo})
      (is (sut/system-id :foo)      {::kind :system ::system-key :foo})
      (is (sut/system-id system-id) {::kind :system ::system-key :foo})
      (is (sut/system-id system)    {::kind :system ::system-key :foo})
      (is (sut/system-id component) {::kind :system ::system-key :foo})
      (is (sut/system-key :foo)      :foo)
      (is (sut/system-key system-id) :foo)
      (is (sut/system-key system)    :foo)
      (is (sut/system-key component) :foo)))
  (testing "System-id? predicate"
    (testing "against right constructors"
      (is (sut/system-id? (sut/system :foo)))
      (is (sut/system-id? (sut/system-id :bar))))
    (testing "against wrong constructors"
      (is (not (sut/system-id? (sut/world))))
      (is (not (sut/system-id? (sut/world-id))))
      (is (not (sut/system-id? (sut/entity :aaa :kkk))))
      (is (not (sut/system-id? (sut/entity-id :aaa))))
      (is (not (sut/system-id? (sut/component :aaa :kkk :sys :t))))
      (is (not (sut/system-id? (sut/component-id :aaa :kkk)))))
    (testing "against non-ids"
      (doseq [x non-ids]
        (is (not (sut/system-id? x))))))
  (testing "system-key function"
    (testing "on keyword"
      (is (= :foo (sut/system-key :foo))))
    (testing "on id"
      (is (= :foo (sut/system-key (sut/system-id :foo)))))
    (testing "on object"
      (is (= :foo (sut/system-key (sut/system :foo))))))
  (testing "system-id function"
    (let [id (sut/system-id :foo)]
      (testing "on keyword"
        (is (= id (sut/system-id :foo))))
      (testing "on id"
        (is (= id (sut/system-id (sut/system-id :foo)))))
      (testing "on object"
        (is (= id (sut/system-id (sut/system :foo)))))
      (testing "on component"
        (is (= id (sut/system-id (sut/component :e :c :foo :typ))))))))

(deftest entity
  (testing "Literal"
    (let [entity-id (sut/entity-id :e)
          entity (sut/entity :e :typ)
          component-id (sut/component-id :e :c)
          component (sut/component :e :c :sys :typ)]
      (is (sut/entity :e :typ)       {::kind :entity ::type :typ ::entity-key :e ::components {}})
      (is (sut/entity-id :e)           {::kind :entity ::entity-key :e})
      (is (sut/entity-id entity-id)    {::kind :entity ::entity-key :e})
      (is (sut/entity-id entity)       {::kind :entity ::entity-key :e})
      (is (sut/entity-id component-id) {::kind :entity ::entity-key :e})
      (is (sut/entity-id component)    {::kind :entity ::entity-key :e})
      (is (sut/entity-key :e)           :e)
      (is (sut/entity-key entity-id)    :e)
      (is (sut/entity-key entity)       :e)
      (is (sut/entity-key component-id) :e)
      (is (sut/entity-key component)    :e)))
  (testing "Entity-id? predicate"
    (testing "against right constructors"
      (is (sut/entity-id? (sut/entity :k :t)))
      (is (sut/entity-id? (sut/entity-id :k))))
    (testing "against wrong constructors"
      (is (not (sut/entity-id? (sut/world))))
      (is (not (sut/entity-id? (sut/world-id))))
      (is (not (sut/entity-id? (sut/system :sys))))
      (is (not (sut/entity-id? (sut/system-id :sys))))
      (is (not (sut/system-id? (sut/component :aaa :kkk :sys :t))))
      (is (not (sut/system-id? (sut/component-id :aaa :kkk)))))
    (testing "against non-ids"
      (doseq [x non-ids]
        (is (not (sut/entity-id? x))))))
  (testing "entity-key function"
    (testing "on keyword"
      (is (= :foo (sut/entity-key :foo))))
    (testing "on id"
      (is (= :foo (sut/entity-key (sut/entity-id :foo)))))
    (testing "on object"
      (is (= :foo (sut/entity-key (sut/entity :foo :typ))))))
  (testing "entity-id function"
    (let [id (sut/entity-id :e)]
      (testing "on keyword"
        (is (= id (sut/entity-id :e))))
      (testing "on id"
        (is (= id (sut/entity-id (sut/entity-id :e)))))
      (testing "on object"
        (is (= id (sut/entity-id (sut/entity :e :t)))))
      (testing "on component"
        (is (= id (sut/entity-id (sut/component :e :c :foo :typ)))))
      (testing "on component-id"
        (is (= id (sut/entity-id (sut/component-id :e :c))))))))

