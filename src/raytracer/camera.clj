(ns raytracer.camera
  (:require [raytracer.matrix :as matrix]))

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
                        :transform matrix/identity-matrix}]
    (merge partial-camera (compute-pixels partial-camera))))

(defn set-transform [camera transform-matrix]
  (assoc camera :transform transform-matrix))

(defn ray-for-pixel [camera x y])

(defn render [camera world])

