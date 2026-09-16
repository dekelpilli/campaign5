(ns campaign5.reliquaries
  (:require
    [campaign5.randoms]
    [campaign5.util :as u]
    [randy.core :as r]
    [randy.rng :as rng]
    [sns.sdk.protocols :as p]
    [sns.sdk.rank :as rank]
    [sns.sdk.vars :as vars]))

(defn- reliquary-actions [reliquary]
  (cond-> []
          (seq reliquary) (conj {:label  "Mythic Shrine of Refinement"
                                 :action ::refinement})
          (or (< (count reliquary) 3)
              (some #(rank/available (:vars %) nil) reliquary))
          (conj {:label  "Mythic Shrine of Annexation"
                 :action ::annexation})))

(defn- reliquary->view-model [id reliquary]
  {:loot/title    "Reliquary"
   :loot/sections [{:section/heading "Mods"
                    :section/items   (mapv u/mod-item reliquary)}]
   :loot/actions  (->> (reliquary-actions reliquary)
                       (mapv (fn [{:keys [label action]}]
                               {:action/label label
                                :action/event [:loot/action {:id     id
                                                             :action action}]})))})

(defn- view-model->reliquary [view-model]
  (into []
        (map (fn [{:item/keys [body vars metadata]}]
               (-> (assoc (u/parse-metadata metadata) :template body)
                   (cond-> (seq vars) (assoc :vars vars)))))
        (get-in view-model [:loot/sections 0 :section/items])))

(defn- resolve-mod [rng mod]
  (update mod :vars #(vars/resolve-vars rng %)))

(defn- new-mod [reliquary-mods {:keys [rng]}]
  (let [idx (rng/next-int rng 0 (count reliquary-mods))]
    (->> (assoc (nth reliquary-mods idx) ::origin idx)
         (resolve-mod rng))))

(def ^:private new-reliquary (comp vector new-mod))

(defn- handle-refinement-shrine [reliquary {:keys [rng]} reliquary-mods]
  (let [idx (rng/next-int rng 0 (count reliquary))
        replacement (r/sample rng reliquary-mods)]
    (-> (subvec reliquary 0 idx)
        (conj replacement)
        (into (subvec reliquary (inc idx))))))

(defn- handle-annexation-shrine [reliquary {:keys [rng]} reliquary-mods]
  (if (< (count reliquary) 3)
    (conj reliquary (r/sample rng reliquary-mods))
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
  p/Generator
  (loot-spec [_]
    {:inputs [{:id      :mods
               :label   "Mods (optional)"
               :type    :enum
               :list?   true
               :options (mapv :template reliquary-mods)}]})
  (generate [_ ctx]
    (->> (generate-reliquary reliquary-mods ctx)
         (reliquary->view-model id)))
  p/Action
  (handle-action [_ {:keys [view-model] :as ctx} action _params]
    (let [reliquary (view-model->reliquary view-model)
          reliquary (case action
                      ::refinement (handle-refinement-shrine reliquary ctx reliquary-mods)
                      ::annexation (handle-annexation-shrine reliquary ctx reliquary-mods))]
      (reliquary->view-model id reliquary))))

(defn -reliquary-generator [config]
  (->> (assoc config :reliquary-mods @u/reliquary-mods)
       map->ReliquaryGenerator))

(comment
  (new-reliquary (u/read-edn-resource "data/reliquary-mods.edn")
                 {:rng @r/default-rng}))
