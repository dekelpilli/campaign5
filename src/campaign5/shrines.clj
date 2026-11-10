(ns campaign5.shrines
  (:require
    [campaign5.randoms]
    [campaign5.util :as u]
    [clojure.string :as str]
    [randy.core :as r]
    [sns.sdk.protocols :as p]))

(defrecord ShrineGenerator [shrines]
  p/Generator
  (loot-spec [_]
    {:inputs [{:id      :name
               :label   "Name (optional)"
               :type    :enum
               :options (mapv :name shrines)}
              {:id      :targets
               :label   "Targets (optional)"
               :type    :enum
               :options ["Vestige" "Legendary" "Ring" "Soul" "Tattoo"]}]})
  (generate [_ {:keys [inputs rng]}]
    (let [filtered-shrines (cond
                             (:name inputs) (filterv (comp #{(:name inputs)} :name) shrines)
                             (:targets inputs) (filterv (fn [{:keys [targets]}]
                                                          (some #{(str/lower-case (:targets inputs))} targets))
                                                        shrines)
                             :else shrines)
          _ (when (empty? filtered-shrines)
              (throw (ex-info "No shrines match filters" inputs)))
          {:keys [name effect cost targets]} (r/sample rng filtered-shrines)]
      {:loot/title    (str "Shrine of " name)
       :loot/sections [{:section/heading "Effect"
                        :section/items   [{:item/body effect}]}
                       {:section/heading "Cost"
                        :section/items   [{:item/body     (str cost)
                                           :item/metadata targets}]}]})))

(defn -shrine-generator [_config]
  (->ShrineGenerator (u/read-edn-resource "data/shrines.edn")))
