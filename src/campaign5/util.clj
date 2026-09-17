(ns campaign5.util
  (:require
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [clojure.string :as str]
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

(def reliquary-mods (delay (read-edn-resource "data/reliquary-mods.edn")))
(def trinkets (delay (read-edn-resource "data/trinkets.edn")))

(def ^:private affinity-metadata-prefix "Affinities: ")
(def ^:private ranking-metadata-regex #"(?:[a-zA-Z ]+: )?\[(?:\d+/\d+)](?:\(\d+\))*")

(defn parse-metadata [metadata]
  {:affinities (into #{}
                     (comp (keep #(when (str/starts-with? % affinity-metadata-prefix)
                                    (subs % (count affinity-metadata-prefix))))
                           (mapcat #(str/split % #", "))
                           (map keyword))
                     metadata)
   :metadata   (filterv
                 #(not (or (str/starts-with? % affinity-metadata-prefix)
                           (re-matches ranking-metadata-regex %)
                           (= % "Randomised")))
                 metadata)})

(defn- affinities->metadata [affinities]
  (->> (mapv name affinities)
       (str/join ", ")
       (str affinity-metadata-prefix)))

(defn- var->metadata [v]
  (let [{:keys [step value points rank]} (if (map? v) v {:value v})]
    (cond-> (format "[%s/%s]" (or step value) (or points 1))
            (> (or rank 1) 1) (str "(" rank ")"))))

(defn- vars->metadata [vars]
  (let [vars (filterv (comp rank/upgradeable? val) vars)]
    (if (= 1 (count vars))
      [(-> vars first val var->metadata)]
      (mapv #(format ["%s: %s"] (vars/humanise-label (key %)) (var->metadata (val %))) vars))))

(defn mod-item
  ([mod] (mod-item mod {}))
  ([{:keys [metadata vars] :as mod} item-vars]
   (let [metadata (cond-> (into (or metadata []) (vars->metadata vars))
                          (seq (:affinities mod)) (conj (affinities->metadata (:affinities mod)))
                          (or (some :random (vals vars))
                              (some :random (vals item-vars))) (conj "Randomised"))]
     (cond-> {:item/body (:template mod)}
             (seq metadata) (assoc :item/metadata metadata)
             (seq vars) (assoc :item/vars vars)))))

(defn choose-by-input [k {:keys [inputs rng]} coll]
  (if (k inputs)
    (some #(when (= (k %) (k inputs)) %) coll)
    (r/sample rng coll)))
