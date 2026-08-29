(ns campaign5.trinkets
  (:require
    [campaign5.randoms]
    [campaign5.util :as u]
    [randy.core :as r]
    [sns.sdk.protocols :as p]))

(defn- trinket->view-model [{:keys [levels]
                             :as   trinket} {:keys [rng]}]
  {:loot/title    "Trinket in the shape of {{shape}}"
   :loot/subtitle "{{info}}"
   :loot/vars     (-> (select-keys trinket [:shape :info])
                      (update-vals (fn [s] {:value    s
                                            :context? true})))
   :loot/sections [{:section/heading "Mods"
                    :section/items   (into []
                                           (comp (take 1) ;TODO take based on level
                                                 (map #(u/mod-item rng %)))
                                           levels)}]})

(defrecord TrinketGenerator [trinkets]
  p/LootGenerator
  (loot-spec [_]
    {:id       :trinkets
     :label    "Trinket"
     :utility? false
     :inputs   [{:id      :shape
                 :label   "Shape (optional)"
                 :type    :enum
                 :options (sort (mapv :shape trinkets))}]})
  (generate [_ {{:keys [shape]} :inputs
                :keys           [rng]
                :as             ctx}]
    (-> (if shape
          (some #(when (= shape (:shape %)) %) trinkets)
          (r/sample rng trinkets))
        (trinket->view-model ctx))))

(defn -trinket-generator [_plugin-config]
  (-> (u/read-edn-resource "data/trinkets.edn")
      ->TrinketGenerator))
