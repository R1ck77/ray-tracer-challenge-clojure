;;; 2D Perlin noise
(ns raytracer.perlin
  (:require [raytracer.svector :as svector]))

(defn- empty-grid [rows columns]
  (into-array (take rows (repeatedly #(make-array java.util.List columns)))))

(defn- random-gradient []
  (svector/normalize (svector/svector (rand) (rand) 0)))

(defn- fill-grid [grid rows columns]
  (doseq [i (range rows)
          j (range columns)]
    (aset grid i j (random-gradient))))

(defn create-grid [rows columns]
  (doto (empty-grid rows columns)
    (fill-grid rows columns)))

(defn- translate-to-positive [p scale]
  p)

(defn- scale-positive-coord [p scale]
  (mod (int p) scale))

(defn- scale-coord [p scale]
  (scale-positive-coord (translate-to-positive p scale)
                        scale))

(defn get-cell [perlin-data [scaled-x scaled-y]]
  (vector (scale-coord scaled-y (:y-scale perlin-data)) 
          (scale-coord scaled-x (:x-scale perlin-data))))

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

(def neighbors-delta [[0 0] [0 1] [1 0] [1 1]])

(defn- combine
  [x-scale y-scale [base-y base-x] [dy dx]]
  (vector (mod (+ base-y dy) y-scale)
          (mod (+ base-x dx) x-scale)))

(defn get-neighbors
  "Return the list of nodes in row,column format"
  [perlin-data scaled-point]  
  (let [{:keys [x-scale y-scale]} perlin-data
        base (get-cell perlin-data scaled-point)]
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
   :dot (svector/dot distance (aget grid y x))})

(defn compute-products
  "Give a grid of gradients and a set of distances, compute a set of dot products"
  [grid distances]
  (vec (map (partial dot-product grid)
            distances)))

(defn- lerp [a0 a1 w]
  (+ (* (- 1 w) a0))
  (* w a1))

(defn interpolate
  "Find the final value for the point

  Note that the coordinates of the scaled point and the ones of the grid are inverted"
  [products scaled-point]

  
  )
