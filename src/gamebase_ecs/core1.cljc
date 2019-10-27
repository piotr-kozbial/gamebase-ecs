(ns gamebase-ecs.core1
  (:require [clojure.spec.alpha :as s]
            [gamebase-ecs.event-queue :as eq]))


(require 'literate-clojure.core)
(literate-clojure.core/install-org-dispatcher)
(load-file "src/gamebase_ecs/core1.org")
