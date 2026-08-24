(ns campaign5.souls
  (:require
    [campaign5.randoms]
    [campaign5.util :as u]
    [randy.core :as r]
    [sns.sdk.progression :as sp]
    [sns.sdk.protocols :as p]
    [sns.sdk.vars :as vars]))

(def ^:private mod-sections
  [[:passive "Passive"] [:proc "Proc"]])

;; A soul's own `:vars` — the damage type it deals, the defence it grants —
;; belong to the whole soul, not to one of its mods: the proc text routinely
;; names the passive's drawn value ("deals 1d8 {{ type }} damage"). They are
;; declared at the top level of a soul in souls.edn, resolved once, and sent as
;; `:loot/vars`, which every template in the view-model can read. A mod's own
;; `:vars` stay on the mod — those are its progression state, levelled
;; separately.
(def ^:private soul-vars
  "The soul-level values, drawn per soul rather than per mod. `:origin`/`:era`
   are declared here rather than in the data because every soul has them."
  {:origin {:random :soul-origin}
   :era    {:random :soul-era}})

(defn- mod-actions [progression soul [section heading]]
  (mapv (fn [{:keys [id]}]
          {:action/label (str "Mythic Shrine of Fulfilment (" heading ": " (vars/humanise-label id) ")")
           :action/event [:loot/action {:id     :souls
                                        :action ::fulfilment
                                        :params {:section section
                                                 :option  id}}]})
        (sp/options-at progression (get soul section))))

(defn- soul->view-model [{:keys [trait vars] :as soul} {:keys [progression]}]
  {:loot/title    "Soul embodying {{ trait }}"
   :loot/vars     (assoc vars :trait {:value trait :context? true})
   :loot/sections (conj (mapv (fn [[section heading]]
                                {:section/heading heading
                                 :section/items   [(sp/mod-item progression (get soul section))]})
                              mod-sections)
                        ;; bodies, not values: origin and era are `:loot/vars`
                        ;; like any other soul-level value, edited once above.
                        {:section/heading "Details"
                         :section/items   [{:item/title "Origin" :item/body "{{ origin }}"}
                                           {:item/title "Era" :item/body "{{ era }}"}]})
   ;TODO add actions for shrine effects (which are random instead of chosen)
   :loot/actions  (into [{:action/label "Mythic Shrine of Soul Transference"
                          :action/event [:loot/action {:id     :souls
                                                       :action ::soul-transference}]}
                         {:action/label "Mythic Shrine of Temporal Shifting"
                          :action/event [:loot/action {:id     :souls
                                                       :action ::temporal-shifting}]}]
                        (mapcat #(mod-actions progression soul %))
                        mod-sections)
   ;; Only the upgrade paths: nothing here is on screen, so nothing here can be
   ;; edited. Everything the DM can see is read back off the view-model.
   :loot/state    {:paths (into {} (map (fn [[section _]] [section (:path (get soul section) [])]))
                                mod-sections)}})

(defn- view-model->soul
  "Rebuild the soul from what is on screen: the displayed trait says which soul
   it is, `:loot/vars` and the item templates carry the DM's edits, and
   `:loot/state` supplies only the upgrade paths, which are displayed nowhere."
  [souls view-model]
  (let [{:keys [paths]} (:loot/state view-model)
        loot-vars (:loot/vars view-model)
        trait     (get-in loot-vars [:trait :value])
        base      (or (some #(when (= trait (:trait %)) %) souls)
                      (throw (ex-info "Unknown soul" {:trait trait})))
        adopt     (fn [soul [idx [section _]]]
                    (let [{:item/keys [body vars]} (get-in view-model [:loot/sections idx :section/items 0])]
                      (assoc soul section (assoc (get base section)
                                                 :template body
                                                 :vars vars
                                                 :path (get paths section [])))))]
    (-> (reduce adopt base (map-indexed vector mod-sections))
        (assoc :trait trait
               :vars (dissoc loot-vars :trait)))))

(defn- take-option
  "Append the chosen upgrade to `section`'s path. An option that is no longer
   available is ignored."
  [progression soul section option-id]
  (if-let [option (->> (sp/options-at progression (get soul section))
                       (some #(when (= option-id (:id %)) %)))]
    (update-in soul [section :path] (fnil conj []) {:id (:id option)})
    soul))

(defn- new-soul [souls {:keys [rng]}]
  (let [soul (r/sample rng souls)]
    (assoc soul :vars (vars/resolve-vars rng (merge soul-vars (:vars soul))))))

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
  (handle-action [_ {:keys [progression rng view-model] :as ctx} action {:keys [section option]}]
    (let [soul (view-model->soul souls view-model)
          soul (case action
                 ::fulfilment (take-option progression soul section option)
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

(comment
  (new-soul (u/read-edn-resource "data/souls.edn")
            {:rng @r/default-rng}))
