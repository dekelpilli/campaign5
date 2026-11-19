(ns generate.vestiges
  (:require [generate.md :as md]))

(defn render [vestiges]
  (md/table ["Depiction" "Info"]
            (map (juxt :depiction :info) (sort-by :depiction vestiges))))
