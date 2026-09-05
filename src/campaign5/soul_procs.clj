(ns campaign5.soul-procs
  "Rolls each character's soul procs over a stretch of combat rounds.

   The ledger of who owns which souls is manual state (`:store/manual`), so the
   UI renders its editor and this only reads it: one row per character, holding
   a record per soul they carry."
  (:require
    [campaign5.util :as u]
    [clojure.string :as str]
    [randy.rng :as rng]
    [sns.sdk.protocols :as p]))

(def ^:private max-rounds 20)

(defn- rounds-input [rounds]
  (-> (if (int? rounds) rounds 3)
      (max 1)
      (min max-rounds)))

(defn- proc? [rng {:keys [proc-chance]}]
  (< (rng/next-int rng 100) (or proc-chance 0)))

(defn- roll-round [rng ledger]
  (into (sorted-map)
        (keep (fn [[char-name souls]]
                (when-let [procced (seq (filterv #(proc? rng %) souls))]
                  ;TODO use map-indexed for case where player has multiple souls of the same trait
                  [char-name (mapv :soul procced)])))
        ledger))

(defn- round-section [idx procs]
  {:section/heading (str "Round " (inc idx))
   :section/items   (if (empty? procs)
                      [{:item/body "Nothing procs."}]
                      (mapv (fn [[char-name souls]]
                              {:item/title char-name
                               :item/body  (str/join ", " souls)})
                            procs))})

(defn- roll-action [id rounds]
  {:action/label (str "Roll " rounds " more round" (when-not (= 1 rounds) "s"))
   :action/event [:loot/action {:id id :action ::roll :params {:rounds rounds}}]})

(defn- view [id rng ledger rounds]
  (if (empty? ledger)
    {:loot/title    "No souls recorded"
     :loot/subtitle "Add each character and the souls they carry above."
     :loot/actions  [(roll-action id rounds)]}
    (let [rolled (vec (repeatedly rounds #(roll-round rng ledger)))
          procs  (transduce (map #(transduce (map count) + 0 (vals %))) + 0 rolled)]
      {:loot/title    (str procs " proc" (when-not (= 1 procs) "s")
                           " over " rounds " round" (when-not (= 1 rounds) "s"))
       :loot/subtitle (str (count ledger) " character" (when-not (= 1 (count ledger)) "s")
                           " carrying " (transduce (map count) + 0 (vals ledger)) " souls")
       :loot/sections (into [] (map-indexed round-section) rolled)
       :loot/actions  [(roll-action id rounds)]})))

(defrecord SoulProcGenerator [id traits]
  p/LootGenerator
  (loot-spec [_]
    {:id             id
     :label          "Soul Procs"
     :utility?       true
     :generate-label "Roll procs"
     :store/manual   {:key-label "Character"
                      :list?     true
                      :fields    [{:id      :soul
                                   :label   "Soul Trait"
                                   :type    :enum
                                   :options traits}
                                  {:id      :proc-chance
                                   :label   "Proc chance (%)"
                                   :type    :int
                                   :default 10}]}
     :inputs         [{:id      :rounds
                       :label   "Combat rounds"
                       :type    :int
                       :default 5}]})
  (generate [_ {:keys [store rng inputs]}]
    (view id rng (p/read-collection store id) (rounds-input (:rounds inputs))))
  p/LootAction
  (handle-action [_ {:keys [store rng]} action {:keys [rounds]}]
    (when-not (= ::roll action)
      (throw (ex-info "Unknown action" {:action action})))
    (view id rng (p/read-collection store id) (rounds-input rounds))))

(defn -soul-proc-generator [{:keys [id]}]
  (->> (u/read-edn-resource "data/souls.edn")
       (mapv :trait)
       (->SoulProcGenerator id)))
