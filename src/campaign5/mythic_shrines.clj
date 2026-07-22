(ns campaign5.mythic-shrines
  (:require
    [campaign5.util :as u]
    [clojure.string :as str]
    [randy.core :as r]
    [sns.spi.protocols :as p]))

(def mythic-shrines (u/read-edn-resource "data/mythic-shrines.edn"))

(defn -mythic-shrine-generator [_plugin-config]
  (reify p/LootGenerator
    (loot-spec [_]
      {:id       :mythic-shrines
       :label    "Mythic Shrines"
       :utility? false
       :inputs   [{:id      :name
                   :label   "Name"
                   :type    :enum
                   :options (mapv :name mythic-shrines)}
                  {:id      :tokens
                   :label   "Tokens"
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
                                             :item/metadata tokens}]}]}))))
