(ns raytracer.camera
  (:require [raytracer.tuple :as tuple]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]
            [raytracer.canvas :as canvas]
            [raytracer.ray :as ray]
            [raytracer.world :as world]))

(def ^:dynamic *single-pixel-rendering* nil) ;;; set to e.g. [30 20] to render a single pixel. Useful for debugging

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
  (for [py (range height)
        px (range width)]
    (vector px py)))

(defn- render-pixel [camera world canvas [px py]]
  (canvas/write canvas
                px py
                (world/color-at world (ray-for-pixel camera px py))))

(defn render-serial [camera world]
  (let [width (:h-size camera)
        height (:v-size camera)]
    (reduce (partial render-pixel camera world)
            (canvas/create-canvas width height)
            (seq-pixels width height))))

(defn- get-pixel-color [camera world [px py]]
  (vector px py (world/color-at world (ray-for-pixel camera px py))))

(defn render
  "very coarse and far from optmized parallel version"
  [camera world]
  (let [width (:h-size camera)
        height (:v-size camera)]
    (reduce (fn write-on-cavas [canvas [x y color]]
              (canvas/write canvas x y color))
            (canvas/create-canvas width height)
            (pmap (partial get-pixel-color camera world)
                  (if *single-pixel-rendering*
                    (vector *single-pixel-rendering*)
                    (seq-pixels width height))))))
