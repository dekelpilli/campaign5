(ns campaign5.souls
  (:require
    [campaign5.randoms] ; register filter
    [campaign5.util :as u]
    [clojure.string :as str]
    [randy.core :as r]
    [sns.spi.protocols :as p]))

(def ^:private soul-types ["aberration" "beast" "celestial" "dragon" "elemental"
                           "fey" "fiend" "giant" "humanoid" "monstrosity"])
(def ^:private soul-eras ["future" "modern" "old" "ancient" "prehistoric"])

(def souls (u/read-edn-resource "data/souls.edn"))

(defn -soul-generator [_plugin-config]
  (reify p/LootGenerator
    (loot-spec [_]
      {:id       :souls
       :label    "Souls"
       :utility? false
       :inputs   []})
    (generate [_ {:keys [rng progression]}]
      (let [{:keys [name passive proc]} (r/sample rng souls)]
        {:loot/title    (str "Soul embodying " name)
         :loot/sections [{:section/heading "Passive"
                          :section/items   [{:item/body (-> (p/current-state progression passive [])
                                                            :effect)}]}
                         {:section/heading "Proc"
                          :section/items   [{:item/body (-> (p/current-state progression proc [])
                                                            :effect)}]}
                         {:section/heading "Details"
                          :section/items   [{:item/title "Type"
                                             :item/body  (str/capitalize (r/sample rng soul-types))}
                                            {:item/title "Era"
                                             :item/body  (str/capitalize (r/sample rng soul-eras))}]}]}))))
