(ns campaign5.legendaries
  (:require
    [campaign5.util :as u]))

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

(defn view-model->legendary [vm]) ;TODO

(defn handle-revealed-potential [leg]
  (update leg :level inc))

(defn handle-discovered-potential [ctx leg]) ;TODO decide on how decision is made to level up vs to add new mod, then implement
