(ns missile-command.terrain
  (:require [clojure.pprint :as pp]))

(defn terrain-height
  ([x t xmin xmax]
   (let [dt (/ (- xmax xmin) (-> t count dec))
         i (int (/ (- x xmin) dt))
         xi (get-in t [i 0])
         yi (get-in t [i 1])
         xf (get-in t [(inc i) 0])
         yf (get-in t [(inc i) 1])]
     (if xf
       (+ yi (* (- yf yi) (/ (- x xi) (- xf xi))))
       yi)))
  ([x t]
   (let [xmin (-> t first first)
         xmax (-> t last first)]
     (cond
       (< x xmin) (-> t first second)
       (> x xmax) (-> t last second)
       :else (terrain-height x t xmin xmax)))))

(defn midpoint-displace [roughness pts]
  (+ (/ (reduce + pts) (count pts)) (* roughness (dec (* 2.0 (rand))))))

(defn subdivide-double-array
  [a lo hi r]
  (when (pos? (dec (- hi lo)))
    (let [m (/ (+ lo hi) 2)]
      (doto a
        (aset m (midpoint-displace r [(aget a lo) (aget a hi)]))
        (subdivide-double-array lo m (* 0.5 r))
        (subdivide-double-array m hi (* 0.5 r))))))

(defn gen-heightmap-1d-array [{:keys [miny maxy octaves roughness]}]
  (let [maxdim (bit-shift-left 1 octaves)]
    (doto (double-array (inc maxdim))
      (aset 0 (double miny))
      (aset maxdim (double maxy))
      (subdivide-double-array 0 maxdim roughness))))

;(defn gen-real [{:keys [minx maxx] :as m}]
;  (vec (let [heights (gen-heightmap-1d-array m)
;             dx (/ (- maxx minx) (dec (count heights)))
;             m (apply min heights)]
;         (sort
;           (zipmap
;             (for [i (range (count heights))] (double (+ minx (* i dx))))
;             (map #(- % m) heights))))))

(defn gen-real [{:keys [minx maxx] :as m}]
  (vec (let [heights (gen-heightmap-1d-array m)
             dx (/ (- maxx minx) (dec (count heights)))]
         (sort
           (zipmap
             (for [i (range (count heights))] (double (+ minx (* i dx))))
             heights)))))