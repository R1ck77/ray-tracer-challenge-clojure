(ns demo.simple-projection
  (:require [raytracer.svector :as svector]
            [raytracer.tuple :as tuple]
            [raytracer.point :as point]
            [raytracer.color :as color]
            [raytracer.matrix :as matrix]
            [raytracer.canvas :as canvas]
            [raytracer.transform :as transform]
            [raytracer.ray :as ray]))

(def canvas-size {:width 32, :height 20})
(def canvas-scale [1 1])

(defn- create-simple-scene
  "Naive setup: the screen position in the scene decides the FOV and VFOV

  The screen orientation is always parallel to XY"
  []
  {:camera (point/point 0 0 -10)
   :objects [{:object (ray/change-transform (ray/sphere) (transform/scale 7 7 7))
              :color [1 0 0]}]
   :screen-z 0})

(defn- adjust-canvas [[x y]]
  (vector (- x (/ (:width canvas-size) 2))
          (- (/ (:height canvas-size) 2) y)))

(defn- scale-pixel [pixel]
  (vec
   (map * canvas-scale (adjust-canvas pixel))))

;;; TODO/FIXME most stuff can probably be cached
(defn- compute-pixel-coordinates [scene-z pixel] 
  (concat (scale-pixel pixel)   
          (vector scene-z 1)))

(defn- seq-ray [scene]
  (let [pixel-to-coord-f (partial compute-pixel-coordinates (:screen-z scene))]
    (for [j (range (:height canvas-size))
          i (range (:width canvas-size))]
      (let [pixel [i j]]
        {:pixel pixel
         :ray (ray/create (:camera scene)
                          (tuple/sub (pixel-to-coord-f pixel)
                                     (:camera scene)))}))))



(defn- compute-pixel [scene canvas {pixel :pixel, ray :ray}]
  ;;; COLOR missing! TODO/FIXME
  (let [t-list (mapcat (fn [{obj :object, color :color}]
                         (map :t (:values (ray/intersect ray obj))))
                       (:objects scene))]
    (if (not (empty? t-list))
      (canvas/write canvas
                    (first pixel) (second pixel)
                    [255 0 0])
      canvas)))

(defn render-scene [scene]
  (spit "output.ppm"
        (canvas/canvas-to-ppm (reduce (partial compute-pixel scene)
                                      (canvas/create-canvas (:width canvas-size)
                                                            (:height canvas-size))
                                      (seq-ray scene)))))


