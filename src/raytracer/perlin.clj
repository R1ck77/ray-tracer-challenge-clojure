;;; 2D Perlin noise
(ns raytracer.perlin
  (:require [raytracer.svector :as svector]))

(def neighbors [[0 0] [1 0] [0 1] [1 1]])

(defn- empty-grid [width height]
  (into-array (take height (repeatedly #(make-array java.util.List width)))))

(defn- random-gradient []
  (svector/normalize (svector/svector (rand) (rand) 0)))

(defn- fill-grid [grid rows columns]
  (doseq [i (range rows)
          j (range columns)]
    (aset grid i j (random-gradient))))

(defn create-grid [columns rows]
  (doto (empty-grid columns rows)
    (fill-grid rows columns)))

(defn create-perlin-data [x-scale y-scale]
  {:x-scale x-scale
   :y-scale y-scale
   :grid (create-grid x-scale y-scale)})

(defn scale-point
  "Scale the point so that it fits inside the proper grid after periodic boundary translation"
  [perlin-data [x y]]
  (vector (* x (:x-scale perlin-data))
          (* y (:y-scale perlin-data))))

(defn- lerp [a0 a1 w]
  (+ (* (- 1 w) a0))
  (* w a1))

(defn- compute-corners [x y]
  (vec
   (map (fn from-delta [[dx dy]]
          (vector (+ (int (Math/floor x)) dx)
                  (+ (int (Math/floor y)) dy)))
        neighbors)))

(defn get-scaled-point-bounds [scaled-point]
  {:corners (apply compute-corners scaled-point)
   :point scaled-point})

(defn get-pbc [perlin-data [x y :as point]]
  (aget (:grid perlin-data)
        (mod y (:y-scale perlin-data))
        (mod x (:x-scale perlin-data))))

(defn- recover-gradients [perlin-data corners]
  (map #(get-pbc perlin-data %) corners))

(defn- compute-distances [perlin-data [x y] corners]
  (map (fn [[cx cy]]
         (vector (- cx x)
                 (- cy y))) corners))

(defn- compute-dot-products [gradients distances]
  (vec
   (map (fn [[gx gy] [dx dy]]
          (+ (* gx dx) (* gy dy))) gradients distances)))

(defn assoc-dot-products [perlin-data {:keys [corners point] :as temp-data}]
  (assoc temp-data
         :dots (compute-dot-products (recover-gradients perlin-data corners)
                                     (compute-distances perlin-data point corners))))

(defn interpolate
  [{:keys [corners point dots]}]
  (let [[px py] point
        distances (vec
                   (map (fn [[cx cy]]
                              (+ (Math/abs (float (- cx px)))
                                 (Math/abs (float (- cy py)))))
                            corners))
        sum-distances (apply + distances)]
    (/ (apply + (map * dots distances))
       sum-distances)))

(defn noise [perlin-data point]
  (interpolate
   (assoc-dot-products perlin-data
                       (get-scaled-point-bounds (scale-point perlin-data point)))))
