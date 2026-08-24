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

(defn add-default-upgrades
  "Give every numeric var an `:inc`-by-its-own-starting-value upgrade, unless
   the mod already declares one that touches it. Only numbers: a drawn var
   (a damage type, a skill) has nothing to increment."
  [{:keys [vars upgrades] :as mod}]
  (let [numeric (into {} (filter (comp number? val)) vars)]
    (if (seq numeric)
      (let [upgrades (or upgrades {:select  :choice
                                   :options []})
            upgraded-ids (into #{}
                               (mapcat (fn [option] (->> (dissoc option :id :repeatable #_:upgrades)
                                                         vals
                                                         (mapcat keys))))
                               (:options upgrades))
            upgrades (update upgrades :options into (comp (remove (comp upgraded-ids key))
                                                          (map (fn [[id value]] {:id  id
                                                                                 :inc {id value}}))) numeric)]
        (assoc mod :upgrades upgrades))
      mod)))

(comment
  (add-default-upgrades
    {:vars     {:dice 1}
     :template "The soul deals {{ dice }}d6 damage to each enemy you can see whose relevant ability score is lower than yours."})
  (add-default-upgrades
    {:vars     {:extra 0}
     :template "Gain temporary hit points equal to your level + {{ extra }}."
     :upgrades {:select  :choice
                :options [{:id  :extra
                           :inc {:extra 2}}]}}))
