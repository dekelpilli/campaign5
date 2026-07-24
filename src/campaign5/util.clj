(ns campaign5.util
  (:require
    [clojure.edn :as edn]
    [clojure.java.io :as io])
  (:import
    (java.io PushbackReader)))

(defn read-edn-resource [resource]
  (-> (io/resource resource)
      io/reader
      PushbackReader.
      edn/read))

(defn add-default-upgrades [{:keys [state upgrades] :as mod}]
  (if (seq state)
    (let [upgrades (or upgrades {:select  :choice
                                 :options []})
          upgraded-ids (into #{}
                             (mapcat (fn [option] (->> (dissoc option :id :repeatable #_:upgrades)
                                                       vals
                                                       (mapcat keys))))
                             (:options upgrades))
          upgrades (update upgrades :options into (comp (remove (comp upgraded-ids key))
                                                        (map (fn [[id value]] {:id  id
                                                                               :inc {id value}}))) state)]
      (assoc mod :upgrades upgrades))
    mod))

(comment
  (add-default-upgrades
    {:state    {:dice 1}
     :template "The soul deals {{ dice }}d6 damage to each enemy you can see whose relevant ability score is lower than yours."})
  (add-default-upgrades
    {:state    {:extra 0}
     :template "Gain temporary hit points equal to your level + {{ extra }}."
     :upgrades {:select  :choice
                :options [{:id  :extra
                           :inc {:extra 2}}]}}))
