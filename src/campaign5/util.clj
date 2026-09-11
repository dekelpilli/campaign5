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

(defn parse-metadata [metadata]
  {:affinities (into #{}
                     (comp (keep #(when (str/starts-with? % "Affinities: ")
                                    (subs % (count "Affinities: "))))
                           (mapcat #(str/split % #", "))
                           (map keyword))
                     metadata)})

(defn- affinities->metadata [affinities]
  (->> (mapv name affinities)
       (str/join ", ")
       (str "Affinities: ")))

(defn mod-item
  ([mod] (mod-item mod {}))
  ([mod item-vars]
   (let [vars (:vars mod)
         metadata (cond-> []
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
