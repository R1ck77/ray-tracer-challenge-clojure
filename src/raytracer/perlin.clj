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

(defn- get-cell-scale [x-scale y-scale x y]
  (vector (scale-coord y y-scale) 
          (scale-coord x x-scale)))

(defn get-cell [perlin-data [x y]]
  (get-cell-scale (:x-scale perlin-data) (:y-scale perlin-data)
                  x y))

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
