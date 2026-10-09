(ns campaign5.tarot
  (:require
    [campaign5.util :as u]
    [clojure.string :as str]
    [randy.core :as r]
    [randy.rng :as rng]
    [sns.sdk.protocols :as p]
    [sns.sdk.rank :as rank]
    [sns.sdk.vars :as vars]))

(defmulti handle-card (fn [_legendary _data _ctx card]
                        (if-let [[_ numeric-card-suffix] (re-matches #"^(?:\d{1,2})(.+)" card)]
                          (str "n" numeric-card-suffix)
                          card)))

(defmethod handle-card :default [_ _ _ card]
  (throw (ex-info (str "No turn in behaviour for " card)
                  {:view-model {:loot/title    (str card " cannot be used for turn ins")
                                :loot/subtitle "Replace it with the card it tells you to draw, then turn in again"}})))

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

(defmethod handle-card "The Hierophant" [legendary _ _ card]
  (update legendary :mods conj
          {:template   "Shrines targeting this item accept any token type."
           :affinities #{:meta}
           :metadata   [(card-origin-meta card)]}))

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
              {:template     "Cannot be targeted by Shrines of Discovered Potential"
               :restriction? true
               :metadata     [(card-origin-meta card)]}
              {:template   "Shrines of Revealed Potential targeting this item are cheaper by 20 tokens."
               :affinities #{:meta}
               :metadata   [(card-origin-meta card)]})))

(defmethod handle-card "Judgement" [legendary _ _ card]
  (update legendary :notes (fnil conj [])
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
        num-downsides (count (filterv zero? (repeatedly 5 #(rng/next-int rng 2))))
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
          {:template     "Cannot be targeted by Shrines of Revealed Potential"
           :restriction? true
           :metadata     [(card-origin-meta card)]}
          {:template   "Shrines of Discovered Potential targeting this item are cheaper by 15 tokens."
           :affinities #{:meta}
           :metadata   [(card-origin-meta card)]}))

(defmethod handle-card "The Sun" [legendary _ _ card]
  (-> (assoc legendary :level 2)
      (update :mods conj
              {:template "This item started at level 2 and its sell price is reduced accordingly."
               :metadata [(card-origin-meta card)]})))

(defmethod handle-card "The Moon" [{:keys [discoverable]
                                    :as   legendary} _ {:keys [rng]} card]
  (let [discoverable-amount-taken (rng/next-int rng 1 (inc (count discoverable)))
        starts-with (->> (r/sample-without-replacement rng discoverable-amount-taken discoverable)
                         (mapv #(update % :metadata (fnil conj []) (card-origin-meta card))))]
    (-> (dissoc legendary :discoverable)
        (update :mods conj
                {:template     "This item cannot be targeted by Shrines."
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

(defmethod handle-card "The World" [legendary _ {:keys [rng registry] :as ctx} card]
  (let [generator-type (r/sample rng [:reliquaries :rings :trinkets])
        generated (p/generate
                    (get registry generator-type)
                    ctx)
        items (-> generated :loot/sections first :section/items)
        mod (case generator-type
              :reliquaries (let [reliquary-mod (first items)]
                             (update reliquary-mod :item/metadata (fnil conj []) "Reliquary modifier"))
              :rings (let [ring (first items)]
                       (-> (dissoc ring :item/title)
                           (update :item/metadata (fnil conj []) (str "From " (:item/title ring)))))
              :trinkets (-> (r/sample rng items)
                            (update :item/metadata (fnil conj []) (str "Trinket depicting "
                                                                       (-> generated :loot/vars :depiction :value)))
                            (update :item/vars assoc :tier {:value    6
                                                            :context? true})))]
    (->> (update mod :item/metadata (fnil conj []) (card-origin-meta card))
         (update legendary :mods conj))))

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

(defmethod handle-card "The Fool" [legendary _ _ card]
  (update legendary :mods conj
          {:template "The sell price of legendary item should be calculated as if only 2 cards were used for its turn in."
           :metadata [(card-origin-meta card)]}))

(defn- numeric-card-n [card]
  (-> (re-find #"^(\d{1,2})" card)
      first
      parse-long))

(defmethod handle-card "n of Cups" [legendary _ _ card]
  (let [n (numeric-card-n card)]
    (update legendary :notes (fnil conj [])
            {:template (format "Based on name alone, the player should choose the result based on %s turn ins of this set." n)
             :metadata [(card-origin-meta card)]})))

(defn- handle-court-of-cups [n legendary card]
  (update legendary :notes (fnil conj [])
          {:template (format "The player should choose the result from %s revealed turn ins of this set." n)
           :metadata [(card-origin-meta card)]}))

(defmethod handle-card "Knight of Cups" [legendary _ _ card]
  (handle-court-of-cups 2 legendary card))
(defmethod handle-card "Queen of Cups" [legendary _ _ card]
  (handle-court-of-cups 3 legendary card))
(defmethod handle-card "King of Cups" [legendary _ _ card]
  (handle-court-of-cups 4 legendary card))

(defmethod handle-card "n of Pentacles" [legendary _ _ card]
  (let [n (numeric-card-n card)]
    (update legendary :mods conj
            {:template   (format "Shrines of Revealed Potential and of Discovered Potential targeting this item are cheaper by %s tokens."
                                 n)
             :affinities #{:meta}
             :metadata   [(card-origin-meta card)]})))

(defn- handle-court-of-pentacles [n legendary card]
  (update legendary :mods conj
          {:template   "Shrines of Revealed Potential and of Discovered Potential targeting this item are cheaper by {{discount}} tokens."
           :vars       {:discount {:value n
                                   :step  5}}
           :affinities #{:meta}
           :metadata   [(card-origin-meta card)]}))

(defmethod handle-card "Knight of Pentacles" [legendary _ _ card]
  (handle-court-of-pentacles 5 legendary card))
(defmethod handle-card "Queen of Pentacles" [legendary _ _ card]
  (handle-court-of-pentacles 8 legendary card))
(defmethod handle-card "King of Pentacles" [legendary _ _ card]
  (handle-court-of-pentacles 10 legendary card))

(defmethod handle-card "n of Swords" [legendary _ {:keys [rng]} card]
  (let [chance (* 0.07 (numeric-card-n card))]
    (update legendary :discoverable
            #(mapv (fn [mod]
                     (let [resolved-vars (vars/resolve-vars rng (:vars mod))]
                       (if-let [upgradeable-keys (rank/available resolved-vars nil)]
                         (if (< (rng/next-double rng 0 1) chance)
                           (-> (assoc mod :vars (rank/rank-up resolved-vars (r/sample rng upgradeable-keys)))
                               (update :metadata (fnil conj []) (str "Upgraded by " card)))
                           mod)
                         mod)))
                   %))))

(defn- handle-court-of-swords [n legendary card]
  (-> (assoc legendary :revealed-discoverable? true)
      (update :notes (fnil conj [])
              {:template (format "The player should choose up to %s of the discoverable mods to upgrade once." n)
               :metadata [(card-origin-meta card)]})))

(defmethod handle-card "Knight of Swords" [legendary _ _ card]
  (handle-court-of-swords 1 legendary card))
(defmethod handle-card "Queen of Swords" [legendary _ _ card]
  (handle-court-of-swords 2 legendary card))
(defmethod handle-card "King of Swords" [legendary _ _ card]
  (handle-court-of-swords 3 legendary card))

(defn- wands-mods [n {legendary-name :name} {:keys [legendaries]} {:keys [rng]} card]
  (let [all-mods (into [] (mapcat
                            (fn [{:keys [name inherent]}]
                              (when-not (= name legendary-name)
                                (into []
                                      (comp (remove :signature?)
                                            (map (fn [mod] (update mod :metadata (fnil conj [])
                                                                   (str "Inherent mod of " name)
                                                                   (str "Option granted by " card)))))
                                      inherent)))
                            legendaries))]
    (into []
          (map-indexed (fn [i v]
                         (update v :template #(format "Option %s: %s" i %))))
          (r/sample-without-replacement rng n all-mods))))

(defn- handle-wands [n legendary data ctx card instruction]
  (let [mods (wands-mods n legendary data ctx card)
        notes (into [{:template   (format instruction n)
                      :title      card
                      :affinities #{:meta}
                      :metadata   [(card-origin-meta card)]}]
                    mods)]
    (update legendary :notes into notes)))

(defmethod handle-card "n of Wands" [legendary data ctx card]
  (-> (numeric-card-n card)
      (handle-wands legendary data ctx card
                    "Player must choose from the %s following mods based on metadata alone.")))

(defn- handle-court-of-wands [n legendary data ctx card]
  (handle-wands n legendary data ctx card
                "Player must choose from the %s following mods."))

(defmethod handle-card "Knight of Wands" [legendary data ctx card]
  (handle-court-of-wands 3 legendary data ctx card))
(defmethod handle-card "Queen of Wands" [legendary data ctx card]
  (handle-court-of-wands 5 legendary data ctx card))
(defmethod handle-card "King of Wands" [legendary data ctx card]
  (handle-court-of-wands 7 legendary data ctx card))

(defn- cards->view-model [id cards]
  {:loot/title    "Tarot Cards"
   :loot/sections (mapv (fn [{:keys [name template vars priority]}]
                          {:section/heading name
                           :section/items   [{:item/body     template
                                              :item/vars     (or vars {})
                                              :item/metadata [(str "Priority: " (or priority 0))]}]})
                        cards)
   :loot/actions  (cond-> []
                          (>= (count cards) 2)
                          (conj {:action/label "Turn in"
                                 :action/event [:loot/action {:id     id
                                                              :action ::turn-in}]}))})

(defn- legendary->view-model [id {:keys [level name
                                         mods discoverable notes
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
                    :section/items   (mapv #(u/mod-item % {:level level}) discoverable)}
                   {:section/heading "Notes"
                    :section/secret? true
                    :section/items   (mapv #(u/mod-item % {:level level}) notes)}]
   ; TODO add shrines
   ; TODO add action to "refresh" the legendary, so that manually added levels/mod changes can be applied to saved history
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
               :options (-> (into []
                                  (comp (map :name)
                                         ; only include main deck cards, non-page/non-ace suited cards can be typed manually
                                        (remove #(re-find #"^(?:King|Queen|Knight)|\d" %))) tarot-cards)
                            (sort))}]})
  (generate [_ {{selected-names :card-names} :inputs}]
    (->> (filterv (comp (set selected-names) :name) tarot-cards)
         (cards->view-model id)))
  p/Action
  (handle-action [this {:keys [view-model] :as ctx} action _]
    (let [legendary (case action
                      ; TODO when https://github.com/spies-and-spiders/companion/issues/19 is done, allow forcing a specific legendary
                      ::turn-in (->> (view-model->cards view-model)
                                     (cards->legendary this ctx)))]
      (legendary->view-model id legendary))))

(defn- expand-tarot-cards [cards]
  (into []
        (mapcat (fn [{:keys [name]
                      :as   card}]
                  (if (str/starts-with? name "%s")
                    (into [{:name     (format name "Ace")
                            :template (format "Draw a card from the %s numbers deck."
                                              (subs name (inc (str/last-index-of name " "))))}]
                          (map (fn [n] (-> (update card :name format n)
                                           (assoc :vars {:n {:value    n
                                                             :context? true}}))))
                          (range 2 11))
                    [card])))
        cards))

(defn -tarot-generator [config]
  (->> (assoc config
              :tarot-cards (-> (u/read-edn-resource "data/tarot-cards.edn")
                               expand-tarot-cards)
              :legendaries (u/read-edn-resource "data/legendaries.edn"))
       map->TarotGenerator))
