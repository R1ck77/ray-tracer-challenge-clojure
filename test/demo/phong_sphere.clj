(ns demo.phong-sphere
  (:require [raytracer.svector :as svector]
            [raytracer.tuple :as tuple]
            [raytracer.point :as point]
            [raytracer.color :as color]
            [raytracer.matrix :as matrix]
            [raytracer.canvas :as canvas]
            [raytracer.transform :as transform]
            [raytracer.ray :as ray]
            [raytracer.intersection :as intersection]
            [raytracer.shapes :as shapes]
            [raytracer.shapes.shared :as shared]
            [raytracer.materials :as materials]
            [raytracer.light-sources :as light-sources]
            [raytracer.phong :as phong]))

(def ^:dynamic canvas-size {:width 320, :height 200})
(def ^:dynamic canvas-scale [0.1 0.1])
(def color (color/color 255 0 0))
(def sphere-template (shapes/change-material (shapes/sphere)
                                             (assoc (materials/material) :color (color/color 1 0.2 1))))

(defn- create-simple-scene
  "Naive setup: the screen position in the scene decides the FOV and VFOV

  The screen orientation is always parallel to XY"
  []
  {:camera (point/point 0 0 -10)
   :object (shapes/change-transform sphere-template
                                    (transform/scale 5 5 5))
   :screen-z 0
   :light-source (light-sources/create-point-light (point/point -10 10 -10)
                                                   (color/color 1 1 1))})

;;; TODO/FIXME most stuff can probably be cached
(defn- compute-pixel-coordinates [scene-z half-width half-height x y] 
  (point/point (* (first canvas-scale) (- x half-width))
               (* (second canvas-scale) (- half-height y))
               scene-z))

(defn- seq-ray [scene]
  (let [pixel-to-coord-f (partial compute-pixel-coordinates
                                  (:screen-z scene)
                                  (/ (:width canvas-size) 2)
                                  (/ (:height canvas-size) 2))]
    (for [j (range (:height canvas-size))
          i (range (:width canvas-size))]
      {:pixel [i j]
       :ray (ray/normalize (ray/ray (:camera scene)
                                    (tuple/sub (pixel-to-coord-f i j)
                                               (:camera scene))))})))

(defn- compute-pixel [object light-source canvas {pixel :pixel, ray :ray}]
  (let [hit (intersection/hit (ray/intersect ray object))]
    (if hit
      (let [point (ray/position ray (:t hit))
            color (phong/lighting object
                                  light-source
                                  point
                                  (.neg (:direction ray))
                                  (shared/compute-normal object point))]
       (canvas/write canvas (first pixel) (second pixel) color))
      canvas)))

(defn render-scene [scene]
  (spit "phong-sphere.ppm"
        (canvas/canvas-to-ppm (reduce (partial compute-pixel (:object scene) (:light-source scene))
                                      (canvas/create-canvas (:width canvas-size)
                                                            (:height canvas-size))
                                      (seq-ray scene)))))

(defn render-demo
  ([width height]
   (let [width-ratio (/ (:width canvas-size) width)
         height-ratio (/ (:height canvas-size) height)]
     (binding [canvas-size {:width width, :height height}
               canvas-scale [(* (first canvas-scale) width-ratio)
                             (* (second canvas-scale) height-ratio)]]
       (render-scene (create-simple-scene))))))
