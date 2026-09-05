(ns campaign5.mythic-shrines
  (:require
    [campaign5.randoms]
    [campaign5.util :as u]
    [clojure.string :as str]
    [randy.core :as r]
    [sns.sdk.protocols :as p]))

(def mythic-shrines (u/read-edn-resource "data/mythic-shrines.edn"))

(defrecord MythicShrineGenerator [id souls]
  p/LootGenerator
  (loot-spec [_]
    {:id       id
     :label    "Mythic Shrines"
     :utility? false
     :inputs   [{:id      :name
                 :label   "Name (optional)"
                 :type    :enum
                 :options (mapv :name mythic-shrines)}
                {:id      :tokens
                 :label   "Tokens (optional)"
                 :type    :enum
                 :options ["Dust" "Legendary" "Ring" "Soul" "Tattoo"]}]})
  (generate [_ {:keys [inputs rng]}]
    (let [filtered-shrines (cond
                             (:name inputs) (filterv (comp #{(:name inputs)} :name) mythic-shrines)
                             (:tokens inputs) (filterv (fn [{:keys [tokens]}]
                                                         (some #{(str/lower-case (:tokens inputs))} tokens))
                                                       mythic-shrines)
                             :else mythic-shrines)
          _ (when (empty? filtered-shrines)
              (throw (ex-info "No shrines match filters" inputs)))
          {:keys [name effect cost tokens]} (r/sample rng filtered-shrines)]
      {:loot/title    (str "Mythic Shrine of " name)
       :loot/sections [{:section/heading "Effect"
                        :section/items   [{:item/body effect}]}
                       {:section/heading "Cost"
                        :section/items   [{:item/body     (str cost)
                                           :item/metadata tokens}]}]})))

(defn -mythic-shrine-generator [{:keys [id]}]
  (->MythicShrineGenerator id (u/read-edn-resource "data/souls.edn")))
