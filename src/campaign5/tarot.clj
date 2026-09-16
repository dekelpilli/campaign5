(ns campaign5.tarot
  (:require
    [campaign5.util :as u]
    [randy.core :as r]
    [randy.rng :as rng]
    [sns.sdk.protocols :as p]))

(defmulti handle-card (fn [_legendary _data _ctx card] card))

(defn- card-origin-meta [card]
  (str "Added by " card))

(defn- legendary-tagged-discoverables [{:keys [discoverable name]}]
  (eduction (map (fn [mod] (assoc mod :metadata [(str "Sourced from " name)]))) discoverable))

(defn- add-random-discoverable [desired-affinity
                                {:keys [name]
                                 :as   legendary}
                                {:keys [legendaries]}
                                {:keys [rng]}
                                card]
  (let [mods (into []
                   (comp (remove (comp #{name} :name))
                         (mapcat legendary-tagged-discoverables)
                         (filter (fn [mod] ((set (:affinities mod)) desired-affinity))))
                   legendaries)
        mod (-> (r/sample rng mods)
                (update :metadata conj (card-origin-meta card)))]
    (update legendary :mods conj mod)))

(defmethod handle-card "The Magician" [legendary data ctx card]
  (add-random-discoverable :resource legendary data ctx card))

(defmethod handle-card "The Empress" [legendary data ctx card]
  (add-random-discoverable :support legendary data ctx card))

(defmethod handle-card "The Emperor" [legendary data ctx card]
  (add-random-discoverable :control legendary data ctx card))

(defmethod handle-card "The High Priestess" [legendary data ctx card]
  (add-random-discoverable :utility legendary data ctx card))

(defmethod handle-card "The Lovers" [legendary data ctx card]
  (add-random-discoverable :survivability legendary data ctx card))

(defmethod handle-card "The Chariot" [legendary data ctx card]
  (add-random-discoverable :tactical legendary data ctx card))

(defmethod handle-card "Strength" [legendary data ctx card]
  (add-random-discoverable :offence legendary data ctx card))

(defmethod handle-card "Wheel of Fortune" [legendary data ctx card]
  (add-random-discoverable :meta legendary data ctx card))

(defmethod handle-card "The Hierophant" [legendary _ _ _]
  (assoc legendary :revealed-discoverable? true))

(defn- downside-mod? [{:keys [restriction? affinities]}]
  (and (not restriction?)
       (empty? affinities)))

(defmethod handle-card "Temperance" [{:keys [mods] :as legendary} _ {:keys [rng]} _]
  (let [downside-idxs (into [] (keep-indexed (fn [idx mod] (when (downside-mod? mod) idx))) mods)]
    (if (seq downside-idxs)
      (update-in legendary [:mods (r/sample rng downside-idxs) :template]
                 #(format "The following mod has been disabled by Temperance: '%s'." %))
      legendary)))

(defmethod handle-card "The Hermit" [legendary _ _ card]
  (-> (dissoc legendary :discoverable)
      (update :mods conj
              {:template     "Cannot be targeted by Mythic Shrines of Discovered Potential"
               :restriction? true
               :metadata     [(card-origin-meta card)]}
              {:template   "Mythic Shrines of Revealed Potential targeting this item are cheaper by 20 tokens."
               :affinities #{:meta}
               :metadata   [(card-origin-meta card)]})))

(defmethod handle-card "Judgement" [legendary _ _ card]
  (update legendary :mods conj
          {:template   "Draw 3 tarot cards after creating this item. Then, either discard this item, or 3 tarot cards."
           :affinities #{:meta}
           :metadata   [(card-origin-meta card)]}))

(defmethod handle-card "The Devil" [{:keys [name]
                                     :as   legendary} {:keys [legendaries]} {:keys [rng]} card]
  (let [{downsides false
         upsides   true} (group-by (comp some? :affinities)
                                   (eduction
                                     (comp (remove (comp #{name} :name))
                                           (mapcat :inherent))
                                     legendaries))
        num-downsides (count (filter zero? (repeatedly 5 #(rng/next-int rng 2))))
        num-upsides (- 5 num-downsides)
        new-discoverable (cond-> []
                                 (pos? num-downsides) (into (r/sample-without-replacement rng num-downsides downsides))
                                 (pos? num-upsides) (into (r/sample-without-replacement rng num-upsides upsides)))]
    (assoc legendary :discoverable
           (mapv
             (fn [mod] (assoc mod :metadata [(card-origin-meta card)]))
             new-discoverable))))

(defmethod handle-card "Death" [{:keys [name]
                                 :as   legendary} {:keys [legendaries]} {:keys [rng]} card]
  (let [{other-name     :name
         other-inherent :inherent} (->> (filterv (comp not #{name} :name) legendaries)
                                        (r/sample rng))]
    (update legendary :mods (fn [mods] (-> (filterv (complement downside-mod?) mods)
                                           (into (comp (filter downside-mod?)
                                                       (map (fn [mod]
                                                              (assoc mod :metadata
                                                                     [(str "Downside from " other-name)
                                                                      (card-origin-meta card)]))))
                                                 other-inherent))))))

(defmethod handle-card "The Hanging Man" [legendary _ _ card]
  (update legendary :mods conj
          {:template     "Cannot be targeted by Mythic Shrines of Revealed Potential"
           :restriction? true
           :metadata     [(card-origin-meta card)]}
          {:template   "Mythic Shrines of Discovered Potential targeting this item are cheaper by 15 tokens."
           :affinities #{:meta}
           :metadata   [(card-origin-meta card)]}))

(defmethod handle-card "The Sun" [legendary _ _ card]
  (-> (assoc legendary :level 2)
      (update :mods conj
              {:template   "This item started at level 2."
               :affinities #{:meta}
               :metadata   [(card-origin-meta card)]})))

(defmethod handle-card "The Moon" [{:keys [discoverable]
                                    :as   legendary} _ {:keys [rng]} card]
  (let [discoverable-amount-taken (rng/next-int rng 1 (inc (count discoverable)))
        starts-with (->> (r/sample-without-replacement rng discoverable-amount-taken discoverable)
                         (mapv #(update % :metadata (fnil conj []) (card-origin-meta card))))]
    (-> (dissoc legendary :discoverable)
        (update :mods conj
                {:template     "This item cannot be targeted by Mythic Shrines."
                 :restriction? true
                 :metadata     [(card-origin-meta card)]})
        (update :mods into starts-with))))

(defmethod handle-card "The Star" [{:keys [name]
                                    :as   legendary} {:keys [legendaries]} {:keys [rng]} card]
  (let [{other-name     :name
         other-inherent :inherent} (->> (filterv (comp not #{name} :name) legendaries)
                                        (r/sample rng))
        signature-mod (some (fn [mod] (when (:signature? mod)
                                        (assoc mod :metadata [(card-origin-meta card)
                                                              (str "Signature mod of " other-name)])))
                            other-inherent)]
    (update legendary :mods conj signature-mod)))

(defmethod handle-card "The World" [legendary data {:keys [rng]} card]
  (let [mod-origin (r/sample rng [:reliquary-mods :rings :trinkets])
        mods (case mod-origin
               :reliquary-mods (:reliquary-mods data)
               :rings [{:name     "My fake ring"
                        :template "My fake ring effect"}] ;TODO pass in rings
               :trinkets (into []
                               (mapcat (fn [{:keys [boons depiction]}]
                                         (eduction
                                           (map (fn [boon]
                                                  (assoc boon :metadata [(str "Trinket depicting " depiction)])))
                                           boons)))
                               (:trinkets data)))
        mod (-> (r/sample rng mods)
                (update :metadata (fnil conj []) (card-origin-meta card)))]
    (update legendary :mods conj mod)))

(defmethod handle-card "Justice" [legendary _ _ card]
  (update legendary :mods conj
          {:template   "Take one of the cards used in this turn in, aside from Justice, from the deck when this item is sold."
           :affinities #{:meta}
           :metadata   [(card-origin-meta card)]}))

(defn- legendary-tagged-signature [{:keys [name inherent]}]
  (-> (some (fn [mod] (when (:signature? mod) mod)) inherent)
      (assoc :metadata [(str "Sourced from " name)])))

(defmethod handle-card "The Tower" [{:keys [name]
                                     :as   legendary} {:keys [legendaries]} {:keys [rng]} card]
  (let [signature-mods (into []
                             (comp (remove (comp #{name} :name))
                                   (map legendary-tagged-signature))
                             legendaries)
        selected-mods (->> (r/sample-without-replacement rng 5 signature-mods)
                           (mapv (fn [mod] (update mod :metadata conj (card-origin-meta card)))))]
    (assoc legendary :discoverable selected-mods)))

(defmethod handle-card "" [legendary {:keys [legendaries]} {:keys [rng]} card]) ;TODO implement courts and numerics

(defn- cards->view-model [id cards]
  {:loot/title    "Tarot Cards"
   :loot/sections (mapv (fn [{:keys [name template priority]}]
                          {:section/heading name
                           :section/items   [{:item/body     template
                                              :item/metadata [(str "Priority: " (or priority 0))]}]})
                        cards)
   :loot/actions  (cond-> []
                          (= 3 (count cards)) (conj {:action/label "Turn in"
                                                     :action/event [:loot/action {:id     id
                                                                                  :action ::turn-in}]}))})

(defn- legendary->view-model [id {:keys [level name mods discoverable
                                         revealed-discoverable?
                                         cards]}]
  {:loot/title    "{{name}} (level {{level}} Legendary Item)"
   :loot/vars     {:name  {:value    name
                           :context? true}
                   :cards {:value    cards
                           :context? true}
                   :level level}
   :loot/subtitle "Made with: {{join (pluck cards \"name\") \", \"}}"
   :loot/sections [{:section/heading "Mods"
                    :section/items   (mapv #(u/mod-item % {:level level}) mods)}
                   {:section/heading "Discoverable mods"
                    :section/secret? (not revealed-discoverable?)
                    :section/items   (mapv #(u/mod-item % {:level level}) discoverable)}]
   ; TODO add shrines
   :loot/actions  []})

(defn- view-model->cards [{:loot/keys [sections]}]
  (mapv
    (fn [{:section/keys [heading items]}]
      (let [{:item/keys [body metadata]} (first items)]
        {:name     heading
         :template body
         :priority (some #(some-> (re-find #"^Priority: (-?\d+)$" %) second parse-long) metadata)}))
    sections))

(defn- prepare-legendary [{:keys [inherent]
                           :as   legendary} cards]
  (-> (assoc legendary
             :level 1
             :cards cards
             :mods (mapv (fn [mod]
                           (->> (cond-> ["Native"]
                                        (:signature? mod) (conj "Signature"))
                                (assoc mod :metadata)))
                         inherent))
      (dissoc :inherent)))

(defn- cards->legendary [{:keys [legendaries]
                          :as   data}
                         {:keys [rng]
                          :as   ctx}
                         cards]
  (let [legendary (-> (r/sample rng legendaries)
                      (prepare-legendary cards))]
    ; TODO return error on The Fool
    (reduce
      (fn [legendary {card-name :name}]
        (handle-card legendary data ctx card-name))
      legendary
      (sort-by :priority cards))))

(defrecord TarotGenerator [id tarot-cards legendaries]
  p/Generator
  (loot-spec [_]
    {:inputs [{:id      :card-names
               :label   "Card name"
               :type    :enum
               :list?   true
               :options (sort (mapv :name tarot-cards))}]})
  (generate [_ {{selected-names :card-names} :inputs}]
    (->> (filterv (comp (set selected-names) :name) tarot-cards)
         (cards->view-model id)))
  p/Action
  (handle-action [this {:keys [view-model] :as ctx} action _]
    (let [legendary (case action
                      ::turn-in (->> (view-model->cards view-model)
                                     (cards->legendary this ctx)))]
      (legendary->view-model id legendary))))

(defn -tarot-generator [config]
  (->> (assoc config
              :tarot-cards (u/read-edn-resource "data/tarot-cards.edn")
              :legendaries (u/read-edn-resource "data/legendaries.edn")
              :reliquary-mods @u/reliquary-mods
              :trinkets @u/trinkets)
       map->TarotGenerator))
