(ns campaign5.util
  (:require
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [clojure.string :as str]
    [randy.core :as r])
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

(defn parse-metadata [metadata]
  {:affinities (into #{}
                     (comp (keep #(when (str/starts-with? % affinity-metadata-prefix)
                                    (subs % (count affinity-metadata-prefix))))
                           (mapcat #(str/split % #", "))
                           (map keyword))
                     metadata)
   :metadata   (filterv
                 #(not (or (str/starts-with? % affinity-metadata-prefix)
                           (= % "Randomised")))
                 metadata)})

(defn- affinities->metadata [affinities]
  (->> (mapv name affinities)
       (str/join ", ")
       (str affinity-metadata-prefix)))

(defn mod-item
  ([mod] (mod-item mod {}))
  ([{:keys [metadata vars] :as mod} item-vars]
   (let [metadata (cond-> (or metadata [])
                          ;TODO metadata for ranks/points/etc
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
