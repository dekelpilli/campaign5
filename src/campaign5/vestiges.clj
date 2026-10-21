(ns campaign5.vestiges
  (:require
    [campaign5.randoms]
    [campaign5.util :as u]
    [sns.sdk.protocols :as p]
    [sns.sdk.rank :as rank]))

(defn- vestige->view-model [id {:keys [boons tier] :as vestige}]
  {:loot/title    "Vestige depicting {{depiction}}"
   :loot/subtitle "{{info}}"
   :loot/vars     (-> (select-keys vestige [:depiction :info])
                      (update-vals (fn [s] {:value    s
                                            :context? true}))
                      (assoc :tier tier))
   :loot/sections [{:section/heading "Boons"
                    :section/items   (mapv u/mod-item boons)}]
   :loot/actions  (cond-> []
                          (rank/available {:tier tier} nil) (conj {:action/label "Shrine of Bestowing"
                                                                   :action/event [:loot/action {:id     id
                                                                                                :action ::bestowing}]}))})

(defn- view-model->vestige [{:loot/keys [vars sections]}]
  (let [boons (into []
                    (comp (filter (comp #{"Boons"} :section/heading))
                          (mapcat :section/items)
                          (map u/item->mod))
                    sections)]
    (-> (select-keys vars [:depiction :info])
        (update-vals :value)
        (assoc
          :tier (:tier vars)
          :boons boons))))

(defrecord VestigeGenerator [id vestiges]
  p/Generator
  (loot-spec [_]
    {:inputs [{:id      :depiction
               :label   "Depiction (optional)"
               :type    :enum
               :options (sort (mapv :depiction vestiges))}]})
  (generate [_ ctx]
    (vestige->view-model id (u/choose-by-input :depiction ctx vestiges)))
  p/Action
  (handle-action [_ {:keys [view-model]} action _]
    (let [vestige (view-model->vestige view-model)]
      (vestige->view-model
        id
        (case action
          ::bestowing (rank/rank-up vestige :tier))))))

(defn- prepare-vestige [vestige]
  (-> (assoc vestige :tier {:value 1
                            :rank  1
                            :max   5})
      (update :boons #(into [] (map-indexed (fn [idx boon]
                                              (update boon :template (fn [tpl] (format "{{#gt tier %s}}%s{{/gt}}" idx tpl))))) %))))

(defn -vestige-generator [{:keys [id]}]
  (->> (u/read-edn-resource "data/vestiges.edn")
       (mapv prepare-vestige)
       (->VestigeGenerator id)))

(comment
  (-> (-vestige-generator {:id :local/test})
      (p/generate {:rng (java.util.Random.)})
      view-model->vestige))
