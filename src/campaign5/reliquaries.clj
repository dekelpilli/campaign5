(ns campaign5.reliquaries
  (:require
    [campaign5.randoms]
    [campaign5.util :as u]
    [randy.core :as r]
    [randy.rng :as rng]
    [sns.sdk.protocols :as p]))

(def ^:private mod-types [:era :origin :other])

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
                                                                              :action ::correction}]})
                          (or (< (count reliquary) 3)
                              (some (comp seq (partial u/options-at progression)) reliquary))
                          (conj {:action/label "Mythic Shrine of Refinement"
                                 :action/event [:loot/action {:id     :reliquaries
                                                              :action ::refinement}]}))
   ;; `:origin` identifies which mod in the data file this is, so its upgrade
   ;; graph can be looked back up; the path is how far it has been refined.
   ;; Neither is visible, so neither can be read off the item.
   :loot/state    {:mods (mapv #(select-keys % [::origin :path]) reliquary)}})

(defn- view-model->reliquary
  "Rebuild the reliquary from the displayed mods — their templates and var
   values as the DM currently has them — over the upgrade graph and path
   `:loot/state` identifies."
  [reliquaries view-model]
  (let [state (get-in view-model [:loot/state :mods] [])]
    (into []
          (map-indexed (fn [i {:item/keys [body vars]}]
                         (let [{::keys [origin] :keys [path]} (get state i)
                               base (get-in reliquaries origin)]
                           (cond-> (assoc base ::origin origin
                                          :path (or path [])
                                          :template body)
                                   (seq vars) (assoc :vars vars)))))
          (get-in view-model [:loot/sections 0 :section/items]))))

(defn- new-mod [reliquaries {:keys [rng]}]
  (let [type (r/sample rng mod-types)
        mods (get reliquaries type)
        idx  (rng/next-int rng 0 (count mods))]
    (assoc (nth mods idx) ::origin [type idx])))

(def ^:private new-reliquary (comp vector new-mod))

(defn- handle-correction-shrine [reliquary {:keys [rng]}]
  (as-> (rng/next-int rng 0 (count reliquary)) idx
    (into (subvec reliquary 0 idx) (subvec reliquary (inc idx)))))

(defn- handle-refinement-shrine [reliquary {:keys [rng progression] :as ctx} reliquaries]
  (if (< (count reliquary) 3)
    (conj reliquary (new-mod reliquaries ctx))
    (let [{:keys [index] :as option} (->> (into []
                                                (comp (map-indexed (fn [idx mod]
                                                                     (mapv #(assoc % :index idx) (u/options-at progression mod))))
                                                      (mapcat identity))
                                                reliquary)
                                          (r/sample rng))]
      (update reliquary index #(u/advance rng % (dissoc option :index))))))

(defrecord ReliquaryGenerator [reliquaries]
  p/LootGenerator
  (loot-spec [_]
    {:id       :reliquaries
     :label    "Reliquaries"
     :utility? false
     ; TODO add mod chooser for loading existing reliquaries once list-typed inputs are available in companion
     :inputs   []})
  (generate [_ ctx]
    (-> (new-reliquary reliquaries ctx)
        (reliquary->view-model ctx)))
  p/LootAction
  (handle-action [_ {:keys [view-model] :as ctx} action _params]
    (let [reliquary (view-model->reliquary reliquaries view-model)
          reliquary (case action
                      ::correction (handle-correction-shrine reliquary ctx)
                      ::refinement (handle-refinement-shrine reliquary ctx reliquaries))]
      (reliquary->view-model reliquary ctx))))

(defn- initialise-reliquaries-data [reliquaries]
  (update-vals reliquaries (fn [mods] (mapv u/add-default-upgrades mods))))

(defn -reliquary-generator [_plugin-config]
  (-> (u/read-edn-resource "data/reliquaries.edn")
      initialise-reliquaries-data
      ->ReliquaryGenerator))

(comment
  (new-reliquary (u/read-edn-resource "data/reliquaries.edn")
                 {:rng @r/default-rng}))
