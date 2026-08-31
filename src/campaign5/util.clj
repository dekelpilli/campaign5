(ns campaign5.util
  (:require
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [clojure.string :as str]
    [randy.core :as r]
    [sns.sdk.progression :as sp]
    [sns.sdk.protocols :as p]
    [sns.sdk.vars :as vars])
  (:import
    (java.io PushbackReader)))

(defn read-edn-resource [resource]
  (-> (io/resource resource)
      io/reader
      PushbackReader.
      edn/read))

;; --- our mod shape ---------------------------------------------------------
;; Souls and reliquaries both keep a levelling mod as
;; `{:vars … :template … :upgrades … :path …}` and read theirs back off the
;; displayed item, so the vars on screen are the current state. These three
;; steps between such a mod and the view-model are shared by both.

(defn parse-metadata [metadata]
  {:affinities (into []
                     (comp (keep #(when (str/starts-with? % "Affinities: ")
                                    (subs % (count "Affinities: "))))
                           (mapcat #(str/split % #", "))
                           (map keyword))
                     metadata)})

(defn- affinities->metadata [affinities]
  (->> (mapv name affinities)
       (str/join ", ")
       (str "Affinities: ")
       vector))

(defn mod-item
  "`mod` as an `sns.sdk.schema/item`: our `:template` as the item body, its vars
   as `:item/vars`, for the browser to render one against the other."
  [rng mod]
  (let [vars (vars/resolve-vars rng (:vars mod))]
    (cond-> {:item/body (:template mod)}
            (seq (:affinities mod)) (assoc :item/metadata (affinities->metadata (:affinities mod)))
            (seq vars) (assoc :item/vars vars))))

(defn options-at
  "The upgrade options available to `mod` as its next step, or nil at a terminal
   node. A mod with no `:path` yet sits at the root."
  [progression mod]
  (:options (p/level-options progression mod (:path mod []))))

(defn advance
  "Take `option` on `mod`: move its vars by that one upgrade, and record the step
   so `options-at` knows where in the graph it now sits. The path is history
   here — each step is paid for as it is taken."
  [rng mod option]
  (-> mod
      (update :vars #(sp/apply-ops rng % option))
      (update :path (fnil conj []) {:id (:id option)})))

(defn add-default-upgrades
  [{:keys [vars upgrades] :as mod}]
  (let [numeric (into {} (filter (comp number? val)) vars)]
    (if (seq numeric)
      (let [upgrades (or upgrades {:select  :choice
                                   :options []})
            upgraded-ids (into #{}
                               (mapcat (fn [option] (->> (dissoc option :id :repeatable #_:upgrades)
                                                         vals
                                                         (mapcat keys))))
                               (:options upgrades))
            upgrades (update upgrades :options into (comp (remove (comp upgraded-ids key))
                                                          (map (fn [[id value]] {:id  id
                                                                                 :inc {id value}}))) numeric)]
        (assoc mod :upgrades upgrades))
      mod)))

(defn- choose-by-input [k {:keys [inputs rng]} coll]
  (if (k inputs)
    (some #(when (= (k %) (k inputs)) %) coll)
    (r/sample rng coll)))

(comment
  (add-default-upgrades
    {:vars     {:dice 1}
     :template "The soul deals {{ dice }}d6 damage to each enemy you can see whose relevant ability score is lower than yours."})
  (add-default-upgrades
    {:vars     {:extra 0}
     :template "Gain temporary hit points equal to your level + {{ extra }}."
     :upgrades {:select  :choice
                :options [{:id  :extra
                           :inc {:extra 2}}]}}))
