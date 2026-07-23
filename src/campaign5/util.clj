(ns campaign5.util
  (:require
    [clojure.edn :as edn]
    [clojure.java.io :as io])
  (:import
    (java.io PushbackReader)))

(defn read-edn-resource [resource]
  (-> (io/resource resource)
      io/reader
      PushbackReader.
      edn/read))
