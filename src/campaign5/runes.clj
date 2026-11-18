(ns campaign5.runes
  (:require
    [campaign5.util :as u]
    [randy.core :as r]
    [randy.rng :as rng]
    [sns.sdk.protocols :as p]))

(def ^:private nato-alphabet ["alfa" "bravo" "charlie" "delta" "echo" "foxtrot" "golf" "hotel" "india" "juliett" "kilo"
                              "lima" "mike" "november" "oscar" "papa" "quebec" "romeo" "sierra" "tango" "uniform"
                              "victor" "whiskey" "x-ray" "yankee" "zulu"])

(defn- rune->view-model [id {:keys [present available inspiration character]}]
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
   ;TODO
   ; Replace mods (takes 2x nato alphabet input, replaces all mods with id=<first> with id=<second> (taken from loaded runes) and attempts to bring them up to points investment of original)
   ; Weaving (add random mod)
   ; Merging (add chosen mod)
   ; Honing (upgrade random mod)
   :loot/actions  []})

(defn- add-rune-mod [rng {:keys [available]
                          :as   rune}]
  (let [idx (rng/next-int rng 0 (count available))
        mod (nth available idx)]
    (-> (assoc rune :available (-> (subvec available 0 idx)
                                   (into (subvec available (inc idx)))))
        (update :present (fnil conj []) mod))))

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
  (handle-action [_ {:keys [view-model]} action _]))

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
