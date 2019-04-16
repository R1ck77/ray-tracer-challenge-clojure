(ns raytracer.camera
  (:require [raytracer.tuple :as tuple]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]
            [raytracer.canvas :as canvas]
            [raytracer.ray :as ray]))

(defn- compute-pixels [partial-camera]
  (let [half-view (Math/tan (/ (:fov partial-camera) 2.0))
        aspect (/ (:h-size partial-camera)
                  (:v-size partial-camera))
        half-width (if (>= aspect 1) half-view (* half-view aspect))
        half-height (if (>= aspect 1) (/ half-view aspect) half-view)]
    {:pixel-size (/ (* half-width 2) (:h-size partial-camera))
     :half-width half-width
     :half-height half-height}))

(defn camera [h-size v-size field-of-view]
  (let [partial-camera {:h-size h-size
                        :v-size v-size
                        :fov field-of-view
                        :transform matrix/identity-matrix
                        :inverse-transform matrix/identity-matrix}]
    (merge partial-camera (compute-pixels partial-camera))))

(defn set-transform [camera transform-matrix]
  (let [inverse-transform (matrix/invert transform-matrix 4)]
    (merge camera
           {:transform transform-matrix
            :inverse-transform inverse-transform})))

(defn- compute-offset [pixel-size v]
  (* (+ v 0.5)
     pixel-size))

(defn ray-for-pixel [camera x y]
  (let [world-x (- (:half-width camera)
                   (compute-offset (:pixel-size camera) x))
        world-y (- (:half-height camera)
                   (compute-offset (:pixel-size camera) y))
        pixel (matrix/transform (:inverse-transform camera)
                                (point/point world-x world-y -1))
        origin (matrix/transform (:inverse-transform camera)
                                 point/origin)]
    (ray/ray origin
             (svector/normalize (tuple/sub pixel origin)))))

(defn- seq-pixels [width height]
  (for [j (range height)
        i (range width)]
    (vector i j)))

(defn- render-pixel [canvas [px py]]
  )

(defn render [camera world]
  (let [width (:h-size camera)
        height (:v-size camera)]
    (reduce render-pixel
            (canvas/create-canvas width height)
            (seq-pixels width height))))
