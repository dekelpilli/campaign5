(ns campaign5.souls
  (:require
    [campaign5.randoms]
    [campaign5.util :as u]
    [clojure.string :as str]
    [randy.core :as r]
    [sns.sdk.protocols :as p]
    [sns.sdk.randoms :as randoms]))

(def ^:private mod-sections
  [[:passive "Passive"] [:proc "Proc"]])

(defn- effect [progression mod]
  (:effect (p/current-state progression mod (:path mod []))))

(defn- options-at [progression mod]
  (:options (p/level-options progression mod (:path mod []))))

(defn- humanise [kw]
  (-> (name kw)
      (str/replace "-" " ")
      str/capitalize))

(defn- mod-actions [progression soul [section heading]]
  (mapv (fn [{:keys [id]}]
          {:action/label (str "Progress " heading ": " (humanise id))
           :action/event [:loot/action {:id     :souls
                                        :action :progress
                                        :params {:section section
                                                 :option  id
                                                 ;; souls are not persisted, so the path travels with the item
                                                 :soul    soul}}]})
        (options-at progression (get soul section))))

(defn- soul->view-model [{soul-name            :name
                          {:keys [origin era]} :details
                          :as                  soul}
                         {:keys [progression]}]
  {:loot/title    (str "Soul embodying " soul-name)
   :loot/sections (conj (mapv (fn [[section heading]]
                                {:section/heading heading
                                 :section/items   [{:item/body (effect progression (get soul section))}]})
                              mod-sections)
                        {:section/heading "Details"
                         :section/items   [{:item/title "Origin"
                                            :item/body  (str/capitalize origin)}
                                           {:item/title "Era"
                                            :item/body  (str/capitalize era)}]})
   :loot/actions  (into [] (mapcat #(mod-actions progression soul %)) mod-sections)})

(defn- take-option
  "Append the chosen upgrade to `section`'s path. An option that is no longer
   available is ignored."
  [progression soul section option-id]
  (if-let [option (->> (options-at progression (get soul section))
                       (some #(when (= option-id (:id %)) %)))]
    (update-in soul [section :path] (fnil conj []) {:id (:id option)})
    soul))

(defn- random-details [rng]
  {:origin (randoms/sample-preset rng :soul-origin)
   :era    (randoms/sample-preset rng :soul-era)})

(defn- new-soul
  "Draw a soul, resolving its `{{x|random:…}}` filters while leaving the
   `{{state}}` placeholders for progression to fill in."
  [souls {:keys [rng render]}]
  (-> (r/sample rng souls)
      (assoc :details (random-details rng))
      (update-in [:passive :template] render {})
      (update-in [:proc :template] render {})))

(defrecord SoulGenerator [souls]
  p/LootGenerator
  (loot-spec [_]
    {:id       :souls
     :label    "Soul"
     :utility? false
     :inputs   []})
  (generate [_ ctx]
    (-> (new-soul souls ctx)
        (soul->view-model ctx)))
  p/LootAction
  (handle-action [_ {:keys [progression] :as ctx} _action {:keys [section option soul]}]
    (-> (take-option progression soul section option)
        (soul->view-model ctx))))

(defn- initialise-souls-data [souls]
  (mapv
    (fn [soul]
      (-> (update soul :passive u/add-default-upgrades)
          (update :proc u/add-default-upgrades)))
    souls))

(defn -soul-generator [_plugin-config]
  (-> (u/read-edn-resource "data/souls.edn")
      initialise-souls-data
      ->SoulGenerator))

(comment
  (new-soul (u/read-edn-resource "data/souls.edn")
            {:rng @r/default-rng :render (fn [template _] template)}))
