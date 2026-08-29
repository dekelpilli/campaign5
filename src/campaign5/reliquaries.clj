(ns campaign5.reliquaries
  (:require
    [campaign5.randoms]
    [campaign5.util :as u]
    [randy.core :as r]
    [randy.rng :as rng]
    [sns.sdk.protocols :as p]))

;; Only the progression bookkeeping travels in `:loot/state`; the mods themselves
;; are read back off the displayed items (`view-model->reliquary`), so a DM's
;; edits are what the next shrine operates on.
(defn- reliquary->view-model [reliquary {:keys [progression rng]}]
  {:loot/title    "Reliquary"
   :loot/sections [{:section/heading "Mods"
                    :section/items   (mapv (partial u/mod-item rng) reliquary)}]
   :loot/actions  (cond-> []
                          (seq reliquary) (conj {:action/label "Mythic Shrine of Correction"
                                                 :action/event [:loot/action {:id     :reliquaries
                                                                              :action ::refinement}]})
                          (or (< (count reliquary) 3)
                              (some (comp seq (partial u/options-at progression)) reliquary))
                          (conj {:action/label "Mythic Shrine of Refinement"
                                 :action/event [:loot/action {:id     :reliquaries
                                                              :action ::annexation}]}))
   ;; `:origin` identifies which mod in the data file this is, so its upgrade
   ;; graph can be looked back up; the path is how far it has been refined.
   ;; Neither is visible, so neither can be read off the item.
   :loot/state    {:mods (mapv #(select-keys % [::origin :path]) reliquary)}})

(defn- view-model->reliquary
  "Rebuild the reliquary from the displayed mods — their templates and var
   values as the DM currently has them — over the upgrade graph and path
   `:loot/state` identifies."
  [reliquary-mods view-model]
  (let [state (get-in view-model [:loot/state :mods] [])]
    (into []
          (map-indexed (fn [i {:item/keys [body vars metadata]}]
                         (let [{::keys [origin] :keys [path]} (get state i)
                               base (nth reliquary-mods origin)]
                           (-> (assoc base ::origin origin
                                      :path (or path [])
                                      :template body)
                               (into (u/parse-metadata metadata))
                               (cond-> (seq vars) (assoc :vars vars))))))
          (get-in view-model [:loot/sections 0 :section/items]))))

(defn- new-mod [reliquary-mods {:keys [rng]}]
  (let [idx (rng/next-int rng 0 (count reliquary-mods))]
    (assoc (nth reliquary-mods idx) ::origin idx)))

(def ^:private new-reliquary (comp vector new-mod))

(defn- handle-refinement-shrine [reliquary {:keys [rng] :as ctx} reliquary-mods]
  (let [idx (rng/next-int rng 0 (count reliquary))
        replacement (new-mod reliquary-mods ctx)]
    (-> (subvec reliquary 0 idx)
        (conj replacement)
        (into (subvec reliquary (inc idx))))))

(defn- handle-annexation-shrine [reliquary {:keys [rng progression] :as ctx} reliquary-mods]
  (if (< (count reliquary) 3)
    (conj reliquary (new-mod reliquary-mods ctx))
    (let [{:keys [index] :as option} (->> (into []
                                                (comp (map-indexed (fn [idx mod]
                                                                     (mapv #(assoc % :index idx) (u/options-at progression mod))))
                                                      (mapcat identity))
                                                reliquary)
                                          (r/sample rng))]
      (update reliquary index #(u/advance rng % (dissoc option :index))))))

(defn- mod-inputs->reliquary [reliquary-mods mods]
  (let [by-template (into {}
                          (map-indexed (fn [idx mod]
                                         [(:template mod) (assoc mod ::origin idx)]))
                          reliquary-mods)]
    (into [] (keep by-template) mods)))

(defn- generate-reliquary [reliquary-mods {:keys [inputs] :as ctx}]
  (if-let [mods (seq (:mods inputs))]
    (mod-inputs->reliquary reliquary-mods mods)
    (new-reliquary reliquary-mods ctx)))

(defrecord ReliquaryGenerator [reliquary-mods]
  p/LootGenerator
  (loot-spec [_]
    {:id       :reliquaries
     :label    "Reliquaries"
     :utility? false
     :inputs   [{:id      :mods
                 :label   "Mods (optional)"
                 :type    :enum
                 :list?   true
                 :options (mapv :template reliquary-mods)}]})
  (generate [_ ctx]
    (-> (generate-reliquary reliquary-mods ctx)
        (reliquary->view-model ctx)))
  p/LootAction
  (handle-action [_ {:keys [view-model] :as ctx} action _params]
    (let [reliquary (view-model->reliquary reliquary-mods view-model)
          reliquary (case action
                      ::refinement (handle-refinement-shrine reliquary ctx reliquary-mods)
                      ::annexation (handle-annexation-shrine reliquary ctx reliquary-mods))]
      (reliquary->view-model reliquary ctx))))

(defn- initialise-reliquaries-data [reliquary-mods]
  (mapv u/add-default-upgrades reliquary-mods))

(defn -reliquary-generator [_plugin-config]
  (-> (u/read-edn-resource "data/reliquary-mods.edn")
      initialise-reliquaries-data
      ->ReliquaryGenerator))

(comment
  (new-reliquary (u/read-edn-resource "data/reliquary-mods.edn")
                 {:rng @r/default-rng}))
