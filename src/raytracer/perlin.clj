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

(defn get-cell [perlin-data [x y]]
  (vector (int (* y (:y-scale perlin-data)))
          (int (* x (:x-scale perlin-data)))))
