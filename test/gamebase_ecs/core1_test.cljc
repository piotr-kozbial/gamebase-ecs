(ns gamebase-ecs.core1-test
  (:require [gamebase-ecs.core1 :as sut]
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
      ;; TODO: component
      )
    (testing "against id constructors"
      (is (sut/id? (sut/world-id)))
      (is (sut/id? (sut/system-id :abc)))
      (is (sut/id? (sut/entity-id :abc)))
      ;; TODO: component
      )
    (testing "against non-id's"
      (doseq [x non-ids]
        (is (not (sut/id? x))))))
  (testing "To-id conversion"
    (testing "from id constructors, against id constructors (idempotency)"
      (doseq [x [(sut/world-id)
                 (sut/system-id :xyz)
                 (sut/entity-id :xyz)
                 ;; TODO: component
                 ]]
        (is (= (sut/to-id x) x))))
    (testing "from object constructors, against id constructors"
      (is (= (sut/to-id (sut/world)) (sut/world-id)))
      (is (= (sut/to-id (sut/system :xyz)) (sut/system-id :xyz)))
      (is (= (sut/to-id (sut/entity :xyz :T)) (sut/entity-id :xyz)))
      ;; TODO: component
      )))

(deftest world
  (testing "World-id? predicate"
    (testing "against right constructors"
      (is (sut/world-id? (sut/world)))
      (is (sut/world-id? (sut/world-id))))
    (testing "against wrong constructors"
      (is (not (sut/world-id? (sut/system :xyz))))
      (is (not (sut/world-id? (sut/system-id :xyz))))
      (is (not (sut/world-id? (sut/entity :aaa :kkk))))
      (is (not (sut/world-id? (sut/entity-id :aaa))))
      ;; TODO component
      )
    (testing "against non-ids"
      (doseq [x non-ids]
        (is (not (sut/world-id? x)))))

    ))

(deftest system
  (testing "System-id? predicate"
    (testing "against right constructors"
      (is (sut/system-id? (sut/system :foo)))
      (is (sut/system-id? (sut/system-id :bar))))
    (testing "against wrong constructors"
      (is (not (sut/system-id? (sut/world))))
      (is (not (sut/system-id? (sut/world-id))))
      (is (not (sut/system-id? (sut/entity :aaa :kkk))))
      (is (not (sut/system-id? (sut/entity-id :aaa))))
      ;; TODO component
      )
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
        ;; TODO
        ))


    )
  )
