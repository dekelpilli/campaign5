(ns campaign5.util)

(defn extract-format-tags [tag-value]
  (when tag-value
    (re-seq #"(?:[^:\"]|\"[^\"]*\")+" tag-value)))
