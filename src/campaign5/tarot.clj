(ns campaign5.tarot
  (:require
    [campaign5.util :as u]
    [randy.core :as r]
    [sns.sdk.protocols :as p]))

(defn- cards->view-model [id cards]
  {:loot/title    "Tarot Cards"
   :loot/sections (mapv (fn [{:keys [name template]}]
                          {:section/heading name
                           :section/items   [{:item/body template}]})
                        cards)
   :loot/actions  (cond-> []
                          (= 3 (count cards)) (conj {:action/label "Turn in"
                                                     :action/event [:loot/action {:id     id
                                                                                  :action ::turn-in}]}))})

(defn- legendary->view-model [id {:keys [level name inherent discovered discoverable
                                         cards]}]
  {:loot/title    "{{name}} (level {{level}} Legendary Item)"
   :loot/vars     {:name  {:value    name
                           :context? true}
                   :cards {:value    cards
                           :context? true}
                   :level level}
   :loot/subtitle "Made with {{cards[0]}}, {{cards[1]}}, and {{cards[2]}}"
   :loot/sections [{:section/heading "Inherent mods"
                    :section/items   (mapv #(u/mod-item % {:level level}) inherent)}
                   {:section/heading "Discovered mods"
                    :section/items   (mapv #(u/mod-item % {:level level}) discovered)}
                   {:section/heading "Discoverable mods"
                    :section/secret? true
                    :section/items   (mapv #(u/mod-item % {:level level}) discoverable)}]
   ;TODO add shrines
   :loot/actions  []})

(defn- view-model->cards [{:loot/keys [sections]}]
  (mapv
    (fn [{:section/keys [heading items]}]
      {:name     heading
       :template (-> items first :item/body)})
    sections))

(defn- cards->legendary [rng {:keys [legendaries]} _cards]
  (-> (r/sample rng legendaries)
      (assoc :level 1)))

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
  (handle-action [this {:keys [rng view-model]} action _]
    (let [legendary (case action
                      ::turn-in (->> (view-model->cards view-model)
                                     (cards->legendary rng this)))]
      (legendary->view-model id legendary))))

(defn -tarot-generator [config]
  (->> (assoc config
              :tarot-cards (u/read-edn-resource "data/tarot-cards.edn")
              :legendaries (u/read-edn-resource "data/legendaries.edn"))
       map->TarotGenerator))
