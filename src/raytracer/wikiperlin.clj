;;; 2D Perlin noise. Cowardly adapted from wikipedia
(ns raytracer.wikiperlin
  (:require [raytracer.svector :as svector]))

(defn- empty-grid [width height]
  (into-array (take height (repeatedly #(make-array java.util.List width)))))

(defn- random-simm []
  (- (rand 2) 1))

(defn- random-gradient []
  (svector/normalize (svector/svector (random-simm) (random-simm) 0)))

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

(defn lerp
  "Function to linearly interpolate between a0 and a1. Weight w should be in the range [0.0, 1.0]"
  [a0 a1 w]
  (+ (* a0 (- 1 w))
     (* a1 w)))

(defn dot-grid-gradient [perlin-data ix iy x y]
  "Computes the dot product of the distance and gradient vectors"
  (let [dx (- x (float ix))
        dy (- y (float iy))
        gradient (aget (:grid perlin-data) iy ix)]
    (+ (* dx (first gradient))
       (* dy (second gradient)))))

(defn perlin
  "Compute Perlin noise at coordinates x, y"
  [perlin-data x y]
  (let [x0 (int x)
        x1 (inc x0)
        y0 (int y)
        y1 (inc y0)]
    (let [sx (- x (float x0))
          sy (- y (float y0))]
      (let [ix0 (lerp (dot-grid-gradient perlin-data x0 y0 x y)
                      (dot-grid-gradient perlin-data x1 y0 x y)
                      sx)
            ix1 (lerp (dot-grid-gradient perlin-data x0 y1 x y)
                      (dot-grid-gradient perlin-data x1 y1 x y)
                      sx)]
        (lerp ix0 ix1 sy)))))
