(ns campaign5.legendaries
  (:require
    [sns.sdk.protocols :as p]))

(defn- view-model->legendary [view-model])

(defrecord TarotGenerator [id tarot-cards legendaries]
  p/Generator
  (loot-spec [_])
  (generate [_ {{tarot-generator :tarot} :registry}]
    {:loot/title  "Must be generated via a tarot card turn ins"
     :loot/error? true})
  p/Action
  (handle-action [this {{{:keys [legendaries]
                          :as   tarot-generator} :tarot} :registry
                        :keys                            [view-model]
                        :as                              ctx}
                  action _]
    (let [legendary (view-model->legendary view-model)])))
