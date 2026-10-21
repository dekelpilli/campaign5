(ns campaign5.legendaries
  (:require
    [campaign5.util :as u]
    [randy.core :as r]
    [sns.sdk.rank :as rank]))
;TODO ensure all legendaries are prepared for at least level 3
(defn- actions [mods]
  (let [restrictions (into [] (comp (filter :restriction?)
                                    (map :template))
                           mods)]
    (if (some #{"This item cannot be targeted by Shrines."} restrictions)
      []
      (cond-> []
              (not (some #{"Cannot be targeted by Shrines of Revealed Potential."} restrictions)) (conj {:action ::revealed-potential
                                                                                                         :label  "Shrine of Revealed Potential"})
              (not (some #{"Cannot be targeted by Shrines of Discovered Potential."} restrictions)) (conj {:action ::discovered-potential
                                                                                                           :label  "Shrine of Discovered Potential"})))))

(defn legendary->view-model [id {:keys [level name
                                        mods discoverable notes
                                        revealed-discoverable?
                                        cards]}]
  {:loot/title    "{{name}} (level {{level}} Legendary Item)"
   :loot/vars     {:name  {:value    name
                           :context? true}
                   :cards {:value    cards
                           :context? true}
                   :level {:value level
                           :step  1}}
   :loot/subtitle "Made with: {{join (pluck cards \"name\") \", \"}}"
   :loot/sections [{:section/heading "Mods"
                    :section/items   (mapv #(u/mod-item % {:level level}) mods)}
                   {:section/heading "Discoverable mods"
                    :section/secret? (not revealed-discoverable?)
                    :section/items   (mapv #(u/mod-item % {:level level}) discoverable)}
                   {:section/heading "Notes"
                    :section/secret? true
                    :section/items   (mapv #(u/mod-item % {:level level}) notes)}]
   :loot/actions  (->> (actions mods)
                       (mapv (fn [{:keys [label action]}]
                               {:action/label label
                                :action/event [:loot/action {:id     id
                                                             :action action}]})))})

(def ^:private restriction-templates
  #{"This item cannot be targeted by Shrines."
    "Cannot be targeted by Shrines of Revealed Potential."
    "Cannot be targeted by Shrines of Discovered Potential."})

(defn- item->mod [{:item/keys [body] :as item}]
  (cond-> (u/item->mod item)
          (restriction-templates body) (assoc :restriction? true)))

(defn view-model->legendary [{:loot/keys [vars sections]}]
  (let [by-heading (into {} (map (juxt :section/heading identity)) sections)
        mods (fn [heading] (mapv item->mod (get-in by-heading [heading :section/items])))]
    {:level                  (get-in vars [:level :value])
     :name                   (get-in vars [:name :value])
     :cards                  (get-in vars [:cards :value])
     :mods                   (mods "Mods")
     :discoverable           (mods "Discoverable mods")
     :notes                  (mods "Notes")
     :revealed-discoverable? (not (get-in by-heading ["Discoverable mods" :section/secret?]))}))

(defn handle-revealed-potential [leg]
  (update leg :level inc))

(defn handle-discovered-potential [{:keys [rng]} {:keys [mods discoverable]
                                                  :as   legendary}]
  (let [upgradeable-mods (into []
                               (map-indexed
                                 (fn [idx mod]
                                   (when-let [vars (rank/available mod nil)]
                                     {:mod-idx idx
                                      :type    :upgrade
                                      :vars    vars})))
                               mods)
        {:keys [type mod-idx]
         :as   upgrade} (->> (into upgradeable-mods
                                   (map-indexed (fn [idx _] {:type    :discover
                                                             :mod-idx idx}))
                                   discoverable)
                             (r/sample rng))]
    (case type
      :upgrade (update-in legendary [:mods mod-idx :vars]
                          rank/rank-up
                          (r/sample rng (:vars upgrade)))
      :discover (-> (update legendary :mods conj (nth discoverable mod-idx))
                    (assoc :discoverable (into (subvec discoverable 0 mod-idx)
                                               (subvec discoverable (inc mod-idx))))))))
