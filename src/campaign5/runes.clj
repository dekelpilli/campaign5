(ns campaign5.runes
  (:require
    [campaign5.util :as u]
    [randy.core :as r]
    [randy.rng :as rng]
    [sns.sdk.protocols :as p]
    [sns.sdk.rank :as rank]
    [sns.sdk.vars :as vars]))

(def ^:private nato-alphabet ["alfa" "bravo" "charlie" "delta" "echo" "foxtrot" "golf" "hotel" "india" "juliett" "kilo"
                              "lima" "mike" "november" "oscar" "papa" "quebec" "romeo" "sierra" "tango" "uniform"
                              "victor" "whiskey" "x-ray" "yankee" "zulu"])

(def ^:private max-mods 4)

(defn- mod-id [mod] (get-in mod [:vars :id :value]))

(defn- nato-input [id label]
  {:id      id
   :label   label
   :type    :enum
   :options nato-alphabet})

(defn- rune-actions [id {:keys [present available]}]
  (let [event   (fn [action] [:loot/action {:id id :action action}])
        room?   (< (count present) max-mods)
        honing? (and (not room?) (some #(rank/available (:vars %) nil) present))]
    (-> (cond-> []
                (and room? (seq available)) (conj {:action/label "Shrine of Weaving"
                                                   :action/event (event ::weaving)}
                                                  {:action/label  "Shrine of Merging"
                                                   :action/event  (event ::merging)
                                                   :action/inputs [(nato-input :mod "Mod")]})
                honing? (conj {:action/label "Shrine of Honing"
                               :action/event (event ::honing)}))
        (conj {:action/label  "Shrine of Reimagining"
               :action/event  (event ::reimagining)
               :action/inputs [(nato-input :from "Old mod")
                               (nato-input :to "New mod")]}))))

(defn- rune->view-model [id {:keys [present available inspiration character] :as rune}]
  {:loot/title    "{{character}}'s {{inspiration}} Rune"
   :loot/vars     {:character   {:value    character
                                 :context? true}
                   :inspiration {:value    inspiration
                                 :context? true}}
   :loot/sections [{:section/heading "Mods"
                    :section/items   (mapv u/mod-item present)}
                   {:section/heading "Remaining mods"
                    :section/secret? true
                    :section/items   (mapv u/mod-item available)}]
   :loot/actions  (rune-actions id rune)})

(defn- view-model->rune [{:loot/keys [vars sections]}]
  (let [mods (fn [heading]
               (into [] (comp (filter (comp #{heading} :section/heading))
                              (mapcat :section/items)
                              (map u/item->mod))
                     sections))]
    {:character   (get-in vars [:character :value])
     :inspiration (get-in vars [:inspiration :value])
     :present     (mods "Mods")
     :available   (mods "Remaining mods")}))

(defn- add-mod-at [{:keys [available] :as rune} idx]
  (-> (assoc rune :available (-> (subvec available 0 idx)
                                 (into (subvec available (inc idx)))))
      (update :present (fnil conj []) (nth available idx))))

(defn- add-rune-mod [rng {:keys [available] :as rune}]
  (add-mod-at rune (rng/next-int rng 0 (count available))))

(defn- merge-mod [{:keys [available] :as rune} id]
  (if-let [idx (first (keep-indexed #(when (= id (mod-id %2)) %1) available))]
    (add-mod-at rune idx)
    rune))

(defn- hone-rune [rng {:keys [present] :as rune}]
  (let [idxs (into [] (keep-indexed #(when (rank/available (:vars %2) nil) %1)) present)]
    (if (seq idxs)
      (let [idx (r/sample rng idxs)]
        (update-in rune [:present idx :vars]
                   rank/rank-up (r/sample rng (rank/available (:vars (nth present idx)) nil))))
      rune)))

(defn- invested [{:keys [vars]}]
  (transduce (map (fn [[_ {:keys [rank points]}]] (* (dec (or rank 1)) (or points 1)))) + 0 vars))

(defn- raise-to [rng points mod]
  (let [affordable (filterv #(<= (get-in mod [:vars % :points] 1) (- points (invested mod)))
                            (rank/available (:vars mod) nil))]
    (if (seq affordable)
      (recur rng points (update mod :vars rank/rank-up (r/sample rng affordable)))
      mod)))

(defn- reimagine [rng inspirations from to {:keys [character inspiration] :as rune}]
  (let [replacement (->> inspirations
                         (some #(when (and (= character (:character %))
                                           (= inspiration (:inspiration %)))
                                  %))
                         :available
                         (some #(when (= to (mod-id %)) %))
                         (#(some-> % (update :vars (partial vars/resolve-vars rng)))))
        swap        (fn [f] (fn [mods] (mapv #(if (= from (mod-id %)) (f %) %) mods)))]
    (if replacement
      (-> (update rune :present (swap #(raise-to rng (invested %) replacement)))
          (update :available (swap (constantly replacement))))
      rune)))

(defrecord RuneGenerator [id rune-inspirations]
  p/Generator
  (loot-spec [_]
    {:inputs [{:id      :character
               :label   "Character"
               :default ""
               :type    :enum
               :options (sort u/characters)}]})
  (generate [_ {:keys [inputs rng]}]
    (if-let [runes (-> (filterv (comp #{(:character inputs)} :character) rune-inspirations)
                       not-empty)]
      (->> (r/sample rng runes)
           (add-rune-mod rng)
           (rune->view-model id))
      {:loot/title  "No rune inspirations found for character '{{}}'"
       :loot/error? true
       :loot/vars   {:character {:value    (:character inputs)
                                 :context? true}}}))
  p/Action
  (handle-action [_ {:keys [rng view-model inputs]} action _]
    (let [rune (view-model->rune view-model)]
      (->> (case action
             ::reimagining (reimagine rng rune-inspirations (:from inputs) (:to inputs) rune)
             ::weaving (add-rune-mod rng rune)
             ::merging (merge-mod rune (:mod inputs))
             ::honing (hone-rune rng rune))
           (rune->view-model id)))))

(defn- prepare-rune-mod [{:keys [id]
                          :as   mod}]
  (-> (dissoc mod :id)
      (assoc-in [:vars :id] {:value    id
                             :context? true})
      (update :metadata (fnil conj []) "ID: {{id}}")))

(defn- prepare-inspiration [character {:keys [mods]
                                       :as   inspiration}]
  (-> (dissoc inspiration :mods)
      (assoc :available (mapv prepare-rune-mod mods)
             :character character)))

(defn- prepare-runes [char-runes]
  (reduce-kv
    (fn [acc character inspirations]
      (into acc
            (map (partial prepare-inspiration character))
            inspirations))
    []
    char-runes))

(defn -rune-generator [{:keys [id]}]
  (->> (u/read-edn-resource "data/runes.edn")
       prepare-runes
       (->RuneGenerator id)))
