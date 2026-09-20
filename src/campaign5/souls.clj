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

(defn- mod-actions [id soul [section heading]]
  (mapv (fn [var-id]
          {:action/label (str "Mythic Shrine of Fulfilment (" heading ": " (vars/humanise-label var-id) ")")
           :action/event [:loot/action {:id     id
                                        :action ::fulfilment
                                        :params {:section section
                                                 :option  var-id}}]})
        (rank/available (:vars (get soul section)) nil)))

(defn- soul->view-model [id {:keys [trait vars] :as soul}]
  {:loot/title    "Soul embodying {{ trait }}"
   :loot/vars     (assoc vars :trait {:value    trait
                                      :context? true})
   :loot/sections (conj (mapv (fn [[section heading]]
                                {:section/heading heading
                                 :section/items   [(u/mod-item (get soul section)
                                                               (dissoc vars :era :origin))]})
                              mod-sections)
                        {:section/heading "Details"
                         :section/items   [{:item/title "Origin"
                                            :item/body  "{{ origin }}"}
                                           {:item/title "Era"
                                            :item/body  "{{ era }}"}]})
   :loot/actions  (into [{:action/label "Refresh"
                          :action/event [:loot/action {:id     id
                                                       :action ::refresh}]}
                         {:action/label "Mythic Shrine of Soul Transference"
                          :action/event [:loot/action {:id     id
                                                       :action ::soul-transference}]}
                         {:action/label "Mythic Shrine of Temporal Shifting"
                          :action/event [:loot/action {:id     id
                                                       :action ::temporal-shifting}]}]
                        (mapcat #(mod-actions id soul %))
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

(defn- take-option [soul section var-id]
  (if (some #{var-id} (rank/available (:vars (get soul section)) nil))
    (update-in soul [section :vars] rank/rank-up var-id)
    soul))

(defn- add-soul-vars [soul]
  (update soul :vars (partial merge base-soul-vars)))

(defn- resolve-soul-vars [rng var]
  (let [resolved-vars (vars/resolve-vars rng var)]
    (update-vals resolved-vars
                 (fn [{:keys  [points rank]
                       ::keys [original-points]
                       :or    {rank 1 points 1}
                       :as    var}]
                   (assoc var
                          ::original-points (or original-points points)
                          :points (+ (or original-points points 1) rank -1))))))

(defn- resolve-mod-vars [rng soul]
  (reduce (fn [soul [section _]] (update-in soul [section :vars] #(resolve-soul-vars rng %)))
          soul
          mod-sections))

(defrecord SoulGenerator [id souls]
  p/Generator
  (loot-spec [_]
    {:inputs [{:id      :trait
               :label   "Trait (optional)"
               :type    :enum
               :options (sort (mapv :trait souls))}]})
  (generate [_ {:keys [rng] :as ctx}]
    (some->> (u/choose-by-input :trait ctx souls)
             add-soul-vars
             (resolve-mod-vars rng)
             (soul->view-model id)))
  p/Action
  (handle-action [_ {:keys [rng view-model]} action {:keys [section option]}]
    (let [soul (view-model->soul souls view-model)
          soul (case action
                 ::refresh soul
                 ::fulfilment (take-option soul section option)
                 ::soul-transference (update soul :vars #(vars/redraw-distinct rng % :origin))
                 ::temporal-shifting (update soul :vars #(vars/redraw-distinct rng % :era)))]
      (->> (resolve-mod-vars rng soul)
           (soul->view-model id)))))

(defn -soul-generator [config]
  (->> (u/read-edn-resource "data/souls.edn")
       (assoc config :souls)
       map->SoulGenerator))
