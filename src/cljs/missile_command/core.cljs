(ns missile-command.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [reagent.core :as reagent :refer [atom]]
            [missile-command.rules :as rules]
            [cljs.core.async :refer [put! chan <! >! timeout close!]]))

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

;http://stackoverflow.com/questions/10298658/mouse-position-inside-autoscaled-svg
(defn click [state e]
  (let [^js/SVGElement svg (.querySelector js/document "svg")
        pt (doto (.createSVGPoint svg)
             (#(-> % .-x (set! (.-clientX e))))
             (#(-> % .-y (set! (.-clientY e)))))
        p (.matrixTransform pt (-> svg .getScreenCTM .inverse))]
    (swap! state update :sams conj (rules/gen-sam @state [(.-x p) (.-y p)]))))

;(swap! state update :sams #(conj % sam))
;https://jsfiddle.net/jaredwilli/qFuDr/
(defn render [state]
  (let [{:keys [targets missiles sams bombs]} @state]
    [:div
     [:svg {:style {:margin "0" :overflow "hidden"
                    :position "fixed"
                    :padding "0px 0px 0px 0px" }
            :width (-> js/window .-innerWidth) :height (-> js/window .-innerHeight)
            :on-click (partial click state)}
      ;[:rect { :x 0 :y 0 :width :100% :height :100% :stroke :black :fill :black}]
      [:rect { :x 0 :y 0
              :width js/innerWidth
              :height js/innerHeight :stroke :black :fill :black}]
      [:polyline {:points rules/t :stroke :cyan :fill :none}]
      (doall (for [{:keys [initial-pos pos] :as sam} sams
                   :when (and initial-pos pos)]
               [:polyline {:key (str "sam-" sam)
                           :points [initial-pos pos] :stroke :green :fill :none}]))
      (doall (for [missile missiles :let [{:keys [initial-pos pos]} missile]]
               [:polyline {:key (str "missile-" missile)
                           :points [initial-pos pos] :stroke :red :fill :none}]))
      (doall (for [target targets]
               [:circle {:key (str "target-" target)
                         :cx (first target)
                         :cy (second target)
                         :r 3 :fill :green}]))
      (doall (for [{:keys [pos radius] :as bomb} bombs]
               [:circle {:key (str "bomb-" bomb)
                         :cx (first pos)
                         :cy (second pos)
                         :r radius :fill :orange}]))]]))

(when-let [app-context (.getElementById js/document "app")]
  (let [state (atom (rules/initial-state))]
  (reagent/render-component
    [render state]
    (do
      (set! (.-onkeydown js/window)
            (fn [e] (when (and (#{32 37 38 39 40} (.-keyCode e))
                               (= (.-target e) (.-body js/document)))
                      (.preventDefault e))))
      (js/setInterval
        #(swap! state (->
                        (into {:world-width js/innerWidth
                               :world-height js/innerHeight})
                        rules/game-step))
        10)
      app-context))))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )
