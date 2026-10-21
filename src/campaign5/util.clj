(ns campaign5.util
  (:require
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [randy.core :as r] [sns.sdk.rank :as rank]
    [sns.sdk.vars :as vars])
  (:import
    (java.io PushbackReader)))

(def characters #{"alice" "bob" "carol" "dave"})

(defn read-edn-resource [resource]
  (-> (io/resource resource)
      io/reader
      PushbackReader.
      edn/read))

(def ^:private affinities-metadata "Affinities: {{join affinities \", \"}}")

(defn- rank-metadata
  "`[step/points]`, then the var's rank once above 1, read off its ranked value."
  [id {:keys [value step points]}]
  (let [id   (name id)
        step (or step value)]
    (format "[%s/%s]{{#gt %s %s}}({{add (divide (subtract %s %s) %s) 1}}){{/gt}}"
            step (or points 1) id value id value step)))

(defn- vars->metadata [vars]
  (let [vars (filterv (comp rank/upgradeable? val) vars)]
    (if (= 1 (count vars))
      [(rank-metadata (ffirst vars) (val (first vars)))]
      (mapv (fn [[id v]] (str (vars/humanise-label id) ": " (rank-metadata id v))) vars))))

(defn mod-item
  ([mod] (mod-item mod {}))
  ([{:keys [metadata vars title affinities] :as mod} item-vars]
   (if-not (contains? mod :item/body)
     (let [metadata (cond-> (into (or metadata []) (vars->metadata vars))
                            (seq affinities) (conj affinities-metadata)
                            (or (some :random (vals vars))
                                (some :random (vals item-vars))) (conj "Randomised"))
           vars     (cond-> vars
                            (seq affinities) (assoc :affinities {:value    (mapv name affinities)
                                                                 :context? true}))]
       (cond-> {:item/body (:template mod)}
               title (assoc :item/title title)
               (seq metadata) (assoc :item/metadata (into [] (distinct) metadata))
               (seq vars) (assoc :item/vars vars)))
     mod)))

(defn item->mod [{:item/keys [title body vars metadata]}]
  (cond-> {:template   body
           :metadata   (or metadata [])
           :affinities (into #{} (map keyword) (get-in vars [:affinities :value]))
           :vars       (not-empty (dissoc vars :affinities))}
          title (assoc :title title)))

(defn choose-by-input [k ctx coll]
  (if-let [kinputs (or (k (:inputs ctx))
                       (k (:action/inputs ctx)))]
    (some #(when (= (k %) kinputs) %) coll)
    (r/sample (:rng ctx) coll)))
