(ns generate.md
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(defn read-edn [path]
  (edn/read-string (slurp path)))

(defn- cell [s]
  (-> (str s)
      (str/replace #"\s*\n\s*" " ")
      (str/replace "|" "\\|")
      (str/replace "{" "&#123;")
      (str/replace "}" "&#125;")
      (str/replace "<" "&lt;")))

(defn table [headers rows]
  (let [row (fn [cells] (str "| " (str/join " | " (map cell cells)) " |"))]
    (str/join "\n" (concat [(row headers)
                            (row (map (constantly "---") headers))]
                           (map row rows)))))
