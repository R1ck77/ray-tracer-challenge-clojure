(ns demo.simple-projection
  (:require [raytracer.svector :as svector]
            [raytracer.tuple :as tuple]
            [raytracer.point :as point]
            [raytracer.matrix :as matrix]
            [raytracer.canvas :as canvas]
            [raytracer.transform :as transform]
            [raytracer.ray :as ray]))

(def canvas-size {:width 320, :height 200})
(def canvas-scale [0.1 0.1])

(defn- create-simple-scene
  "Naive setup: the screen position in the scene decides the FOV and VFOV

  The screen orientation is always parallel to XY"
  []
  {:camera (point/point -10 0 0)
   :objects [(ray/sphere)]
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
  (let [pixel-to-coord-f (partial compute-pixel-coordinates (:scene-z scene))]
    (for [i (range (:width canvas-size))
          j (range (:height canvas-size))]
      {:pixel [i j]
       :ray (ray/create (:camera scene)
                        (svector/norm (tuple/neg (pixel-to-coord-f scene)
                                                 (:camera scene))))})))


(defn- compute-pixel [scene canvas {pixel :pixel, ray :ray}]
  
  )

(defn render-scene [scene]
  (reduce (partial compute-pixel scene)
          (canvas/create-canvas (:width canvas-size)
                                (:height canvas-size))
          (seq-ray scene)))


