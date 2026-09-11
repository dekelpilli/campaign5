(ns campaign5.trinkets
  (:require
    [campaign5.randoms]
    [campaign5.util :as u]
    [sns.sdk.protocols :as p]
    [sns.sdk.rank :as rank]))

(defn- trinket->view-model [id {:keys [boons tier] :as trinket}]
  {:loot/title    "Trinket depicting {{depiction}}"
   :loot/subtitle "{{info}}"
   :loot/vars     (-> (select-keys trinket [:depiction :info])
                      (update-vals (fn [s] {:value    s
                                            :context? true}))
                      (assoc :tier tier))
   :loot/sections [{:section/heading "Boons"
                    :section/items   (mapv u/mod-item boons)}]
   :loot/actions  (cond-> []
                          (rank/available {:tier tier} nil) (conj {:action/label "Mythic Shrine of Bestowing"
                                                                   :action/event [:loot/action {:id     id
                                                                                                :action ::bestowing}]}))})

(defn- view-model->trinket [{:loot/keys [vars sections]}]
  (let [boons (into []
                    (comp (filter (comp #{"Boons"} :section/heading))
                          (mapcat :section/items)
                          (map (fn [{:item/keys [body metadata]}]
                                 (assoc
                                   (u/parse-metadata metadata)
                                   :template body))))
                    sections)]
    (-> (select-keys vars [:depiction :info])
        (update-vals :value)
        (assoc
          :tier (:tier vars)
          :boons boons))))

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
    (trinket->view-model id (u/choose-by-input :depiction ctx trinkets)))
  p/LootAction
  (handle-action [_ {:keys [view-model]} action _]
    (let [trinket (view-model->trinket view-model)]
      (trinket->view-model
        id
        (case action
          ::bestowing (rank/rank-up trinket :tier))))))

(defn- prepare-trinket [trinket]
  (-> (assoc trinket :tier {:value 1
                            :rank  1
                            :max   5})
      (update :boons #(into [] (map-indexed (fn [idx boon]
                                              (update boon :template (fn [tpl] (format "{{#gt tier %s}}%s{{/gt}}" idx tpl))))) %))))

(defn -trinket-generator [{:keys [id]}]
  (->> (u/read-edn-resource "data/trinkets.edn")
       (mapv prepare-trinket)
       (->TrinketGenerator id)))

(comment
  (-> (-trinket-generator {:id :local/test})
      (p/generate {:rng (java.util.Random.)})
      view-model->trinket))
