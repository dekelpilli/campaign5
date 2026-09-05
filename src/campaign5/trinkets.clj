(ns campaign5.trinkets
  (:require
    [campaign5.randoms]
    [campaign5.util :as u]
    [sns.sdk.protocols :as p]))

(defn- trinket->view-model [{:keys [boons level]
                             :as   trinket}
                            {:keys [rng]}]
  {:loot/title    "Trinket depicting {{depiction}}"
   :loot/subtitle "{{info}}"
   :loot/vars     (-> (select-keys trinket [:depiction :info :level])
                      (update-vals (fn [s] {:value    s
                                            :context? true})))
   :loot/sections [{:section/heading "Mods"
                    :section/items   (->> (subvec boons 0 level)
                                          (mapv #(u/mod-item rng %)))}]
   :loot/state    {:upgrades (subvec boons level)}
   :loot/actions  (cond-> []
                          (< level 5) (conj {:action/label "Mythic Shrine of Bestowing"
                                             :action/event [:loot/action {:id     :trinkets
                                                                          :action ::bestowing}]}))})

(defn- view-model->trinket [{:loot/keys [vars state sections]}]
  (let [boons (into []
                    (comp (filter (comp #{"Mods"} :section/heading))
                          (mapcat :section/items)
                          (map (fn [{:item/keys [body metadata]}]
                                 (assoc
                                   (u/parse-metadata metadata)
                                   :template body))))
                    sections)]
    (-> (select-keys vars [:depiction :info :level])
        (update-vals :value)
        (assoc :boons (into boons (:upgrades state))))))

(defn- upgrade-trinket-level [trinket]
  (update trinket :level (fn [level] (min (inc level) 5))))

(defrecord TrinketGenerator [id trinkets]
  p/LootGenerator
  (loot-spec [_]
    {:id       id
     :label    "Trinkets"
     :utility? false
     :inputs   [{:id      :depiction
                 :label   "Depiction (optional)"
                 :type    :enum
                 :options (sort (mapv :depiction trinkets))}]})
  (generate [_ ctx]
    (-> (u/choose-by-input :depiction ctx trinkets)
        (trinket->view-model ctx)))
  p/LootAction
  (handle-action [_ {:keys [view-model] :as ctx} action _]
    (let [trinket (view-model->trinket view-model)
          trinket (case action
                    ::bestowing (upgrade-trinket-level trinket))]
      (trinket->view-model trinket ctx))))

(defn -trinket-generator [{:keys [id]}]
  (->> (u/read-edn-resource "data/trinkets.edn")
       (mapv #(assoc % :level 1))
       (->TrinketGenerator id)))
