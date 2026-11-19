(ns generate.shrines
  (:require [clojure.string :as str]
            [generate.md :as md]))

(defn render [shrines]
  (md/table ["Shrine" "Cost" "Targets" "Effect"]
            (map (fn [{:keys [name cost targets effect]}]
                   [name cost (str/join ", " targets) effect])
                 shrines)))
