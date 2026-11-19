(ns generate.tarot
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [generate.md :as md]))

(def suits ["Cups" "Pentacles" "Swords" "Wands"])

(defn- suit [name]
  (some #(when (str/ends-with? name (str " of " %)) %) suits))

(defn- placeholders [s]
  (-> s
      (str/replace #"\{\{times (\d+) n\}\}" "$1n")
      (str/replace "{{n}}" "n")))

(defn- expand [{:keys [name] :as card}]
  (if (str/starts-with? name "%s")
    (let [s (suit name)]
      [{:name     (format name "Ace")
        :template (format "Draw a card from the %s numbers deck." s)}
       (assoc card :name (format name "2–10"))])
    [card]))

(defn- rows [cards]
  (map (fn [{:keys [name template priority]}]
         [name (placeholders template) (or priority 0)])
       cards))

(defn render
  "All groups under `##` headings, or a single group's table when `group` is given."
  [cards group]
  (let [groups (-> (group-by (comp suit :name) (mapcat expand cards))
                   (set/rename-keys {nil "Major Arcana"}))
        table  #(md/table ["Card" "Effect" "Priority"] (rows (get groups %)))]
    (cond
      (nil? group) (str/join "\n\n" (for [g (cons "Major Arcana" suits)]
                                      (str "## " g "\n\n" (table g))))
      (contains? groups group) (table group)
      :else (throw (ex-info (str "Unknown tarot group: " group) {:groups (keys groups)})))))
