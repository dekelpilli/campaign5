(ns campaign5.reliquaries
  (:require
    [campaign5.randoms]
    [campaign5.util :as u]
    [randy.core :as r]
    [randy.rng :as rng]
    [sns.sdk.protocols :as p]
    [sns.sdk.rank :as rank]
    [sns.sdk.vars :as vars]))

(defn- reliquary->view-model [reliquary]
  {:loot/title    "Reliquary"
   :loot/sections [{:section/heading "Mods"
                    :section/items   (mapv u/mod-item reliquary)}]
   :loot/actions  (cond-> []
                          (seq reliquary) (conj {:action/label "Mythic Shrine of Correction"
                                                 :action/event [:loot/action {:id     :reliquaries
                                                                              :action ::refinement}]})
                          (or (< (count reliquary) 3)
                              (some #(rank/available (:vars %) nil) reliquary))
                          (conj {:action/label "Mythic Shrine of Refinement"
                                 :action/event [:loot/action {:id     :reliquaries
                                                              :action ::annexation}]}))
   :loot/state    {:mods (mapv #(select-keys % [::origin]) reliquary)}})

(defn- view-model->reliquary
  "Rebuild the reliquary from the displayed mods — their templates, var values
   and ranks as the DM currently has them — over the data entries
   `:loot/state` identifies."
  [reliquary-mods view-model]
  (let [state (get-in view-model [:loot/state :mods] [])]
    (into []
          (map-indexed (fn [i {:item/keys [body vars metadata]}]
                         (let [{::keys [origin]} (get state i)
                               base (nth reliquary-mods origin)]
                           (-> (assoc base ::origin origin :template body)
                               (into (u/parse-metadata metadata))
                               (cond-> (seq vars) (assoc :vars vars))))))
          (get-in view-model [:loot/sections 0 :section/items]))))

(defn- resolve-mod
  "Draw a mod's declared vars. Ranking reads `:value`, so a mod still holding
   declarations has nothing to offer; from here on it comes back off the
   view-model already resolved."
  [rng mod]
  (update mod :vars #(vars/resolve-vars rng %)))

(defn- new-mod [reliquary-mods {:keys [rng]}]
  (let [idx (rng/next-int rng 0 (count reliquary-mods))]
    (->> (assoc (nth reliquary-mods idx) ::origin idx)
         (resolve-mod rng))))

(def ^:private new-reliquary (comp vector new-mod))

(defn- handle-refinement-shrine [reliquary {:keys [rng] :as ctx} reliquary-mods]
  (let [idx (rng/next-int rng 0 (count reliquary))
        replacement (new-mod reliquary-mods ctx)]
    (-> (subvec reliquary 0 idx)
        (conj replacement)
        (into (subvec reliquary (inc idx))))))

(defn- handle-annexation-shrine
  "Below three mods, annex another; otherwise rank up one of the vars across the
   reliquary that can still take a rank, picked uniformly over all of them."
  [reliquary {:keys [rng] :as ctx} reliquary-mods]
  (if (< (count reliquary) 3)
    (conj reliquary (new-mod reliquary-mods ctx))
    (if-let [choices (seq (into []
                                (comp (map-indexed (fn [idx mod]
                                                     (map #(vector idx %) (rank/available (:vars mod) nil))))
                                      cat)
                                reliquary))]
      (let [[index var-id] (r/sample rng choices)]
        (update-in reliquary [index :vars] rank/rank-up var-id))
      reliquary)))

(defn- mod-inputs->reliquary [reliquary-mods rng mods]
  (let [by-template (into {}
                          (map-indexed (fn [idx mod]
                                         [(:template mod) (assoc mod ::origin idx)]))
                          reliquary-mods)]
    (into [] (comp (keep by-template) (map #(resolve-mod rng %))) mods)))

(defn- generate-reliquary [reliquary-mods {:keys [inputs rng] :as ctx}]
  (if-let [mods (seq (:mods inputs))]
    (mod-inputs->reliquary reliquary-mods rng mods)
    (new-reliquary reliquary-mods ctx)))

(defrecord ReliquaryGenerator [id reliquary-mods]
  p/LootGenerator
  (loot-spec [_]
    {:id       id
     :label    "Reliquaries"
     :utility? false
     :inputs   [{:id      :mods
                 :label   "Mods (optional)"
                 :type    :enum
                 :list?   true
                 :options (mapv :template reliquary-mods)}]})
  (generate [_ ctx]
    (-> (generate-reliquary reliquary-mods ctx)
        reliquary->view-model))
  p/LootAction
  (handle-action [_ {:keys [view-model] :as ctx} action _params]
    (let [reliquary (view-model->reliquary reliquary-mods view-model)
          reliquary (case action
                      ::refinement (handle-refinement-shrine reliquary ctx reliquary-mods)
                      ::annexation (handle-annexation-shrine reliquary ctx reliquary-mods))]
      (reliquary->view-model reliquary))))

(defn -reliquary-generator [{:keys [id]}]
  (->> (u/read-edn-resource "data/reliquary-mods.edn")
       (->ReliquaryGenerator id)))

(comment
  (new-reliquary (u/read-edn-resource "data/reliquary-mods.edn")
                 {:rng @r/default-rng}))
