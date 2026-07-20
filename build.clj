(ns build
  (:require
    [clojure.tools.build.api :as b]))

(def lib 'campaign5/campaign5)
(def class-dir "target/classes")
(def uber-file (format "target/%s.jar" (name lib)))

(defn clean [_]
  (b/delete {:path "target"}))

(defn uber [_]
  (clean nil)
  (let [basis (b/create-basis {:project "deps.edn"})]
    (b/copy-dir {:src-dirs   ["src" "resources"]
                 :target-dir class-dir})
    (b/uber {:basis     basis
             :class-dir class-dir
             :uber-file uber-file})
    (println "Built" uber-file)))
