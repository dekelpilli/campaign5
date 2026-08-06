(ns campaign5.reliquaries
  (:require
    [campaign5.randoms]
    [campaign5.util :as u]
    [randy.core :as r]
    [randy.rng :as rng]
    [sns.sdk.protocols :as p]))

(def ^:private mod-types [:era :origin :other])

(defn- effect [progression mod]
  (:effect (p/current-state progression mod (:path mod []))))

(defn- options-at [progression mod]
  (:options (p/level-options progression mod (:path mod []))))

(defn- reliquary->view-model [reliquary {:keys [progression]}]
  {:loot/title    "Reliquary"
   :loot/sections [{:section/heading "Mods"
                    :section/items   (mapv
                                       (fn [mod] {:item/body (effect progression mod)})
                                       reliquary)}]
   :loot/actions  (cond-> []
                          (seq reliquary) (conj {:action/label "Mythic Shrine of Correction"
                                                 :action/event [:loot/action {:id     :reliquaries
                                                                              :action ::correction
                                                                              :params {:reliquary reliquary}}]})
                          (or (< (count reliquary) 3)
                              (some (comp seq (partial options-at progression)) reliquary))
                          (conj {:action/label "Mythic Shrine of Refinement"
                                 :action/event [:loot/action {:id     :reliquaries
                                                              :action ::refinement
                                                              :params {:reliquary reliquary}}]}))})

(defn- new-mod [reliquaries {:keys [rng render]}]
  (let [mod (->> (r/sample rng mod-types)
                 (get reliquaries)
                 (r/sample rng))]
    (update mod :template render {})))

(def ^:private new-reliquary (comp vector new-mod))

(defn- handle-correction-shrine [reliquary {:keys [rng]}]
  (as-> (rng/next-int rng 0 (count reliquary)) idx
    (into (subvec reliquary 0 idx) (subvec reliquary (inc idx)))))

(defn- handle-refinement-shrine [reliquary {:keys [rng progression] :as ctx} reliquaries]
  (if (< (count reliquary) 3)
    (conj reliquary (new-mod reliquaries ctx))
    (let [{:keys [index id]} (->> (into []
                                        (comp (map-indexed (fn [idx mod]
                                                             (mapv #(assoc % :index idx) (options-at progression mod))))
                                              (mapcat identity))
                                        reliquary)
                                  (r/sample rng))]
      (update-in reliquary [index :path] (fnil conj []) {:id id}))))

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
  (handle-action [_ ctx action {:keys [reliquary]}]
    (let [reliquary (case action
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
  (->> (r/sample  mod-types)
       (get (u/read-edn-resource "data/reliquaries.edn"))
       (r/sample))
  (new-reliquary (u/read-edn-resource "data/reliquaries.edn")
                 {:rng @r/default-rng :render (fn [template _] template)}))
