(ns campaign5.souls
  (:require
    [campaign5.randoms :as randoms]
    [campaign5.util :as u]
    [clojure.string :as str]
    [randy.core :as r]
    [sns.spi.protocols :as p]))

(defn- random-details [rng]
  {:origin (randoms/sample-preset rng :soul-origin)
   :era    (randoms/sample-preset rng :soul-era)})

(defn- soul->view-model [{:keys                [passive proc]
                          soul-name            :name
                          {:keys [origin era]} :details
                          :as                  soul}
                         {:keys [progression]}]
  {:loot/title    (str "Soul embodying " soul-name)
   :loot/sections [{:section/heading "Passive"
                    :section/items   [{:item/body (-> (p/current-state progression passive (:path passive [])) ;TODO proper progression handling
                                                      :effect)}]}
                   {:section/heading "Proc"
                    :section/items   [{:item/body (-> (p/current-state progression proc (:path proc [])) ;TODO proper progression handling
                                                      :effect)}]}
                   {:section/heading "Details"
                    :section/items   [{:item/title "Origin"
                                       :item/body  (str/capitalize origin)}
                                      {:item/title "Era"
                                       :item/body  (str/capitalize era)}]}]
   ;TODO centralise progression/state->upgrade actions logic
   :loot/actions  (-> (mapv (fn [proc-kw] (keyword "proc" (name proc-kw))) (keys (:state proc)))
                      (into (map (fn [passive-kw] (keyword "passive" (name passive-kw)))) (keys (:state passive)))
                      (->> (mapv (fn [kw] {:action/label (str "Progress " kw) ; TODO make it nicer
                                           :action/event [:loot/action {:id     :souls
                                                                        :action :progress
                                                                        :params {:section (keyword (namespace kw))
                                                                                 :state   (keyword (name kw))
                                                                                 :soul    soul}}]}))))})

(defn- new-soul [souls rng]
  (-> (r/sample rng souls)
      (assoc :details (random-details rng))
      (update-in [:passive :template] randoms/render-randoms rng)
      (update-in [:proc :template] randoms/render-randoms rng)
      (assoc :details (random-details rng))))

(defrecord SoulGenerator [souls]
  p/LootGenerator
  (loot-spec [_]
    {:id       :souls
     :label    "Soul"
     :utility? false
     :inputs   []})
  (generate [_ {:keys [rng] :as ctx}]
    (-> (new-soul souls rng)
        (soul->view-model ctx)))
  p/LootAction
  (handle-action [_ ctx _action {:keys [section state soul]}]
    (-> (update-in soul [section :state state] inc) ;TODO proper progression handling
        (soul->view-model ctx))))

(defn -soul-generator [_plugin-config]
  (->SoulGenerator (u/read-edn-resource "data/souls.edn")))

(comment
  (new-soul (u/read-edn-resource "data/souls.edn") @randy.core/default-rng))
