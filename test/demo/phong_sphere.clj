(ns demo.phong-sphere
  (:require [raytracer.svector :as svector]
            [raytracer.tuple :as tuple]
            [raytracer.point :as point]
            [raytracer.color :as color]
            [raytracer.matrix :as matrix]
            [raytracer.canvas :as canvas]
            [raytracer.transform :as transform]
            [raytracer.ray :as ray]
            [raytracer.materials :as materials]
            [raytracer.light-sources :as light-sources]
            [raytracer.phong :as phong]))

(def canvas-size {:width 320, :height 200})
(def canvas-scale [0.1 0.1])
(def color [255 0 0])
(def sphere-template (ray/change-material (ray/sphere)
                                          (assoc (materials/material) :color [1 0.2 1])))

(defn- create-simple-scene
  "Naive setup: the screen position in the scene decides the FOV and VFOV

  The screen orientation is always parallel to XY"
  []
  {:camera (point/point 0 0 -10)
   :object (ray/change-transform sphere-template
                                 (transform/scale 5 5 5))
   :screen-z 0
   :light-source (light-sources/create-point-light (point/point -10 10 -10)
                                                   [1 1 1])})

;;; TODO/FIXME most stuff can probably be cached
(defn- compute-pixel-coordinates [scene-z half-width half-height x y] 
  (vector (* (first canvas-scale) (- x half-width))
          (* (second canvas-scale) (- half-height y))
          scene-z
          1))

(defn- seq-ray [scene]
  (let [pixel-to-coord-f (partial compute-pixel-coordinates
                                  (:screen-z scene)
                                  (/ (:width canvas-size) 2)
                                  (/ (:height canvas-size) 2))]
    (for [j (range (:height canvas-size))
          i (range (:width canvas-size))]
      {:pixel [i j]
       :ray (ray/normalize (ray/create (:camera scene)
                                       (tuple/sub (pixel-to-coord-f i j)
                                                  (:camera scene))))})))

(defn- compute-pixel [object light-source canvas {pixel :pixel, ray :ray}]
  (let [hit (ray/hit (:values (ray/intersect ray object)))]
    (if hit
      (let [point (ray/position ray (:t hit))
            color (phong/lighting (:material object)
                                  light-source
                                  point
                                  (svector/neg (:direction ray))
                                  ((:normal object) point))]
       (canvas/write canvas (first pixel) (second pixel) color))
      canvas)))

(defn render-scene [scene]
  (spit "output.ppm"
        (canvas/canvas-to-ppm (reduce (partial compute-pixel (:object scene) (:light-source scene))
                                      (canvas/create-canvas (:width canvas-size)
                                                            (:height canvas-size))
                                      (seq-ray scene)))))


