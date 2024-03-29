(def project 'gamebase-ecs)
(def version "0.1.0-SNAPSHOT")

(set-env! :resource-paths #{"resources" "src"}
          :source-paths   #{"test"}
          :dependencies   '[[org.clojure/clojure "1.10.1"]
                            [org.clojure/spec.alpha "0.2.176"]
                            [adzerk/boot-test "RELEASE" :scope "test"]
                            [orchestra "2019.02.06-1"]
                            [literate-clojure "0.2.6"]])

(task-options!
 pom {:project     project
      :version     version
      :description "FIXME: write description"
      :url         "http://example/FIXME"
      :scm         {:url "https://github.com/yourname/gamebase-ecs"}
      :license     {"Eclipse Public License"
                    "http://www.eclipse.org/legal/epl-v10.html"}})

(deftask build
  "Build and install the project locally."
  []
  (comp (pom) (jar) (install)))

(require '[adzerk.boot-test :refer [test]])
