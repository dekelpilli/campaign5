(ns campaign5.souls
  (:require
    [campaign5.randoms]
    [campaign5.util :as u]
    [sns.sdk.protocols :as p]
    [sns.sdk.vars :as vars]))

(def ^:private mod-sections
  [[:passive "Passive"] [:proc "Proc"]])

(def ^:private base-soul-vars
  {:origin {:random :soul-origins}
   :era    {:random :soul-eras}})

(defn- mod-actions [progression soul [section heading]]
  (mapv (fn [{:keys [id]}]
          {:action/label (str "Mythic Shrine of Fulfilment (" heading ": " (vars/humanise-label id) ")")
           :action/event [:loot/action {:id     :souls
                                        :action ::fulfilment
                                        :params {:section section
                                                 :option  id}}]})
        (u/options-at progression (get soul section))))

(defn- soul->view-model [{:keys [trait vars] :as soul} {:keys [progression rng]}]
  {:loot/title    "Soul embodying {{ trait }}"
   :loot/vars     (assoc vars :trait {:value    trait
                                      :context? true})
   :loot/sections (conj (mapv (fn [[section heading]]
                                {:section/heading heading
                                 :section/items   [(u/mod-item rng (get soul section))]})
                              mod-sections)
                        {:section/heading "Details"
                         :section/items   [{:item/title "Origin"
                                            :item/body  "{{ origin }}"}
                                           {:item/title "Era"
                                            :item/body  "{{ era }}"}]})
   :loot/actions  (into [{:action/label "Mythic Shrine of Soul Transference"
                          :action/event [:loot/action {:id     :souls
                                                       :action ::soul-transference}]}
                         {:action/label "Mythic Shrine of Temporal Shifting"
                          :action/event [:loot/action {:id     :souls
                                                       :action ::temporal-shifting}]}]
                        (mapcat #(mod-actions progression soul %))
                        mod-sections)
   :loot/state    {:paths (into {} (map (fn [[section _]] [section (:path (get soul section) [])]))
                                mod-sections)}})

(defn- view-model->soul [souls view-model]
  (let [{:keys [paths]} (:loot/state view-model)
        loot-vars (:loot/vars view-model)
        trait     (get-in loot-vars [:trait :value])
        base      (or (some #(when (= trait (:trait %)) %) souls)
                      (throw (ex-info "Unknown soul" {:trait trait})))
        adopt     (fn [soul [idx [section _]]]
                    (let [{:item/keys [body vars metadata]} (get-in view-model [:loot/sections idx :section/items 0])
                          section-data (-> (get base section)
                                           (assoc :template body
                                                  :vars vars
                                                  :path (get paths section []))
                                           (into (u/parse-metadata metadata)))]
                      (assoc soul section section-data)))]
    (-> (reduce adopt base (map-indexed vector mod-sections))
        (assoc :trait trait
               :vars (dissoc loot-vars :trait)))))

(defn- take-option
  "Apply the chosen upgrade to `section`'s mod. An option that is no longer
   available is ignored."
  [progression rng soul section option-id]
  (if-let [option (->> (u/options-at progression (get soul section))
                       (some #(when (= option-id (:id %)) %)))]
    (update soul section #(u/advance rng % option))
    soul))

(defn- add-soul-vars [soul rng]
  (update soul :vars (fn [vars] (->> (merge base-soul-vars vars)
                                     (vars/resolve-vars rng)))))

(defrecord SoulGenerator [souls]
  p/LootGenerator
  (loot-spec [_]
    {:id       :souls
     :label    "Souls"
     :utility? false
     :inputs   [{:id      :trait
                 :label   "Trait (optional)"
                 :type    :enum
                 :options (sort (mapv :trait souls))}]})
  (generate [_ ctx]
    (some-> (u/choose-by-input :trait ctx souls)
            (add-soul-vars ctx)
            (soul->view-model ctx)))
  p/LootAction
  (handle-action [_ {:keys [progression rng view-model] :as ctx} action {:keys [section option]}]
    (let [soul (view-model->soul souls view-model)
          soul (case action
                 ::fulfilment (take-option progression rng soul section option)
                 ::soul-transference (update soul :vars #(vars/redraw-distinct rng % :origin))
                 ::temporal-shifting (update soul :vars #(vars/redraw-distinct rng % :era)))]
      (soul->view-model soul ctx))))

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
