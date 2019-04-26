;;; 2D Perlin noise
(ns raytracer.perlin
  (:require [raytracer.svector :as svector]))

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

(defn get-cell [grid x y]
  (aget grid y x))

(defn- scale-coord [p scale]
  (mod (int p) scale))

(defn get-cell-corner [perlin-data [scaled-x scaled-y]]
  (vector (scale-coord scaled-x (:x-scale perlin-data)) 
          (scale-coord scaled-y (:y-scale perlin-data))))

(defn make-positive [scaled-p scale]
  (if (>= scaled-p 0)
    scaled-p
    (- scaled-p (* scale (dec (int (/ scaled-p scale)))))))

(defn- scale-coordinate [p scale]
  (make-positive (* p scale) scale))

(defn scale-point
  "Scale the point so that it fits inside the proper grid after periodic boundary translation"
  [perlin-data [x y]]
  (vector (scale-coordinate x (:x-scale perlin-data))
          (scale-coordinate y (:y-scale perlin-data))))

(def neighbors-delta [[0 0] [1 0] [0 1] [1 1]])

(defn- combine
  [x-scale y-scale [base-x base-y] [dx dy]]
  (vector (mod (+ base-x dx) x-scale)
          (mod (+ base-y dy) y-scale)))

(defn get-neighbors
  "Return the list of nodes"
  [perlin-data scaled-point]  
  (let [{:keys [x-scale y-scale]} perlin-data
        base (get-cell-corner perlin-data scaled-point)]
    (vec (map (partial combine x-scale y-scale base) neighbors-delta))))

(defn compute-distances
  "Compute the distance of a scaled point from the node vertices

  Use svector because… It's there."
  [neighbors [sx sy]]
  (vec (map (fn single-distance [[gx gy :as neighbor]]
              {:coords neighbor
               :distance (svector/svector (- gx sx) (- gy sy) 0)})
            neighbors)))

(defn- dot-product
  [grid
   {[x y :as coords] :coords
    distance :distance}]
  {:coords coords
   :dot (svector/dot distance (get-cell grid x y))})

(defn compute-products
  "Give a grid of gradients and a set of distances, compute a set of dot products"
  [grid distances]
  (vec (map (partial dot-product grid)
            distances)))

(defn- lerp [a0 a1 w]
  (+ (* (- 1 w) a0))
  (* w a1))

(defn interpolate
  "Find the final value for the point"
  [products [x y]]
  (let [x1 (nth products 0)
        x2 (nth products 1)
        x3 (nth products 2)
        x4 (nth products 3)]
    (lerp (lerp (:dot x1) (:dot x2) (- (first (:coords x1)) y))
          (lerp (:dot x3) (:dot x4) (- (first (:coords x3)) y))
          (- (second (:coords x1)) x))))

(defn noise [perlin-data point]
  (let [scaled-point (scale-point perlin-data point)]
    (interpolate (compute-products (:grid perlin-data)
                                   (compute-distances (get-neighbors perlin-data scaled-point)
                                                      scaled-point))
                 scaled-point)))
