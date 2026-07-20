(ns campaign5.souls
  (:require
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [clojure.string :as str]
    [randy.core :as r]
    [sns.spi.protocols :as p])
  (:import
    (java.io PushbackReader)))

(def ^:private soul-types ["aberration" "beast" "celestial" "dragon" "elemental"
                           "fey" "fiend" "giant" "humanoid" "monstrosity"])
(def ^:private soul-eras ["future" "modern" "old" "ancient" "prehistoric"])

(def souls (-> (io/resource "data/souls.edn")
               io/reader
               PushbackReader.
               edn/read))

(defn souls-generator [_plugin-config]
  (reify p/LootGenerator
    (loot-spec [_]
      {:id       :souls
       :label    "Souls"
       :utility? false
       :inputs   []})
    (generate [_ {:keys [rng progression]}]
      (let [{:keys [name passive proc]} (r/sample rng souls)]
        {:loot/title    (str "Soul of a " name)
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
