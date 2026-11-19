(require '[babashka.classpath :as cp]
         '[babashka.fs :as fs]
         '[cheshire.core :as json]
         '[clojure.string :as str])

(cp/add-classpath (str (fs/parent (fs/absolutize *file*))))
(require '[generate.md :as md]
         '[generate.shrines :as shrines]
         '[generate.tarot :as tarot]
         '[generate.vestiges :as vestiges])

(defn- expand [{:keys [tarot-cards shrines vestiges]} content]
  (-> content
      (str/replace #"<!-- tarot-table(?: (.+?))? -->"
                   (fn [[_ group]] (tarot/render tarot-cards group)))
      (str/replace "<!-- shrines-table -->" (shrines/render shrines))
      (str/replace "<!-- vestiges-table -->" (vestiges/render vestiges))))

(defn- walk [f items]
  (mapv (fn [{chapter :Chapter :as item}]
          (if chapter
            {:Chapter (-> chapter
                          (update :content f)
                          (update :sub_items (partial walk f)))}
            item))
        items))

(if (= "supports" (first *command-line-args*))
  (System/exit 0)
  (let [[{:keys [root]} book] (json/parse-string (slurp *in*) true)
        repo (fs/parent root)
        data {:tarot-cards (md/read-edn (str (fs/path repo "resources/data/tarot-cards.edn")))
              :shrines     (-> (md/read-edn (str (fs/path repo "data-plugins/shrines.edn"))) :tables :shrines)
              :vestiges    (md/read-edn (str (fs/path repo "resources/data/vestiges.edn")))}]
    (print (json/generate-string (update book :items (partial walk (partial expand data)))))))
