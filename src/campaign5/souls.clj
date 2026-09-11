(ns campaign5.souls
  (:require
    [campaign5.randoms]
    [campaign5.util :as u]
    [sns.sdk.protocols :as p]
    [sns.sdk.rank :as rank]
    [sns.sdk.vars :as vars]))

(def ^:private mod-sections
  [[:passive "Passive"] [:proc "Proc"]])

(def ^:private base-soul-vars
  {:origin {:random :soul-origins}
   :era    {:random :soul-eras}})

(defn- mod-actions [soul [section heading]]
  (mapv (fn [id]
          {:action/label (str "Mythic Shrine of Fulfilment (" heading ": " (vars/humanise-label id) ")")
           :action/event [:loot/action {:id     :souls
                                        :action ::fulfilment
                                        :params {:section section
                                                 :option  id}}]})
        (rank/available (:vars (get soul section)) nil)))

(defn- soul->view-model [{:keys [trait vars] :as soul}]
  {:loot/title    "Soul embodying {{ trait }}"
   :loot/vars     (assoc vars :trait {:value    trait
                                      :context? true})
   :loot/sections (conj (mapv (fn [[section heading]]
                                {:section/heading heading
                                 :section/items   [(u/mod-item (get soul section) vars)]})
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
                        (mapcat #(mod-actions soul %))
                        mod-sections)})

(defn- view-model->soul [souls view-model]
  (let [loot-vars (:loot/vars view-model)
        trait     (get-in loot-vars [:trait :value])
        base      (or (some #(when (= trait (:trait %)) %) souls)
                      (throw (ex-info "Unknown soul" {:trait trait})))
        adopt     (fn [soul [idx [section _]]]
                    (let [{:item/keys [body vars metadata]} (get-in view-model [:loot/sections idx :section/items 0])
                          section-data (-> (get base section)
                                           (assoc :template body :vars vars)
                                           (into (u/parse-metadata metadata)))]
                      (assoc soul section section-data)))]
    (-> (reduce adopt base (map-indexed vector mod-sections))
        (assoc :trait trait
               :vars (dissoc loot-vars :trait)))))

(defn- take-option
  "Rank up one of `section`'s vars. A var that cannot take another rank is
   ignored."
  [soul section var-id]
  (if (some #{var-id} (rank/available (:vars (get soul section)) nil))
    (update-in soul [section :vars] rank/rank-up var-id)
    soul))

(defn- add-soul-vars [soul]
  (update soul :vars (partial merge base-soul-vars)))

(defn- resolve-mod-vars [rng soul]
  (reduce (fn [soul [section _]] (update-in soul [section :vars] #(vars/resolve-vars rng %)))
          soul
          mod-sections))

(defrecord SoulGenerator [id souls]
  p/LootGenerator
  (loot-spec [_]
    {:id       id
     :label    "Souls"
     :utility? false
     :inputs   [{:id      :trait
                 :label   "Trait (optional)"
                 :type    :enum
                 :options (sort (mapv :trait souls))}]})
  (generate [_ {:keys [rng] :as ctx}]
    (some->> (u/choose-by-input :trait ctx souls)
             add-soul-vars
             (resolve-mod-vars rng)
             soul->view-model))
  p/LootAction
  (handle-action [_ {:keys [rng view-model]} action {:keys [section option]}]
    (let [soul (view-model->soul souls view-model)
          soul (case action
                 ::fulfilment (take-option soul section option)
                 ::soul-transference (update soul :vars #(vars/redraw-distinct rng % :origin))
                 ::temporal-shifting (update soul :vars #(vars/redraw-distinct rng % :era)))]
      (soul->view-model soul))))

(defn -soul-generator [{:keys [id]}]
  (->> (u/read-edn-resource "data/souls.edn")
       (->SoulGenerator id)))
