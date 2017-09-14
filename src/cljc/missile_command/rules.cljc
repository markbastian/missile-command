(ns missile-command.rules
  (:require [missile-command.terrain :as terrain]))

(def t (terrain/gen-real {:roughness 100 :minx 0 :maxx 568 :miny -200 :maxy -100 :octaves 3}))

(defn dist [a b]
  (let [[dx dy] (map - a b)]
    (Math/sqrt (+ (* dx dx) (* dy dy)))))

(defn rand-station [{:keys [world-width] :as state}]
  (update state :targets conj
          (let [x (rand-int world-width)]
            [x (terrain/terrain-height x t)])))

(defn gen-projectile [src dst]
  (let [[dx dy] (mapv - dst src)
        mag (Math/sqrt (+ (* dx dx) (* dy dy)))
        heading [(/ dx mag) (/ dy mag)]]
    {:initial-pos src :pos src :target dst :vel heading}))

(defn gen-missile [{:keys [world-width targets]}]
  (gen-projectile [(rand-int world-width) 0] (rand-nth targets)))

(defn gen-sam [{:keys [targets]} tgt]
  (gen-projectile (apply min-key #(dist % tgt) targets) tgt))

(defn initial-state []
  (let [is {:world-width  568
            :world-height 320
            :targets      []
            :sams         #{}
            :bombs        #{}}
        f (nth (iterate rand-station is) 3)
        missiles (take 5 (repeatedly #(gen-missile f)))]
    (assoc f :missiles missiles)))

(defn sim-missile [state {:keys [pos vel target] :as missile}]
  (if (> (second pos) (second target))
    (gen-missile state)
    (assoc missile :pos (mapv + pos (map #(* 0.2 %) vel)))))

(defn sim-sam [state {:keys [pos vel target] :as sam}]
  (let [s (update state :sams disj sam)]
    (if (< (second pos) (second target))
      (update s :bombs conj {:pos pos :radius 0})
      (update s :sams conj (assoc sam :pos (mapv + pos vel vel))))))

(defn sim-sams [{:keys [sams] :as state}]
  (reduce sim-sam state sams))

(defn in-blast? [{pm :pos} {:keys [pos radius]}]
  (< (dist pos pm) radius))

(defn sim-bombs [{:keys [bombs missiles] :as state}]
  (let [b (map #(update % :radius + 0.8) bombs)
        m (for [missile missiles]
            (if (some (partial in-blast? missile) b)
              (gen-missile state)
              missile))]
    (-> state
        (assoc :bombs (filter #(< (:radius %) 30) b))
        (assoc :missiles m))))

(defn game-step [{:keys [missiles] :as state}]
  (-> state
      (assoc :missiles (map (partial sim-missile state) missiles))
      sim-sams
      sim-bombs))
