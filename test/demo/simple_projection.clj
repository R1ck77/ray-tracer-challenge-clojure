(ns demo.simple-projection
  (:require [raytracer.svector :as svector]
            [raytracer.tuple :as tuple]
            [raytracer.point :as point]
            [raytracer.color :as color]
            [raytracer.matrix :as matrix]
            [raytracer.canvas :as canvas]
            [raytracer.transform :as transform]
            [raytracer.shapes :as shapes]
            [raytracer.ray :as ray]))

(def canvas-size {:width 320, :height 200})
(def canvas-scale [0.1 0.1])
(def color [255 0 0])

(defn- create-simple-scene
  "Naive setup: the screen position in the scene decides the FOV and VFOV

  The screen orientation is always parallel to XY"
  []
  {:camera (point/point 0 0 -10)
   :object (shapes/change-transform (shapes/sphere)
                                    (transform/scale 5 5 5))
   :screen-z 0})

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
       :ray (ray/ray (:camera scene)
                     (tuple/sub (pixel-to-coord-f i j)
                                (:camera scene)))})))

(defn- compute-pixel [object canvas {pixel :pixel, ray :ray}]
  (if (empty?
       (map :t (ray/intersect ray object)))
    canvas
    (canvas/write canvas (first pixel) (second pixel) color)))

(defn render-scene [scene]
  (spit "output.ppm"
        (canvas/canvas-to-ppm (reduce (partial compute-pixel (:object scene))
                                      (canvas/create-canvas (:width canvas-size)
                                                            (:height canvas-size))
                                      (seq-ray scene)))))


