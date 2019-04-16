(ns raytracer.camera
  (:require [raytracer.matrix :as matrix]))

(defn camera [h-size v-size field-of-view]
  {:h-size h-size
   :v-size v-size
   :fov field-of-view
   :transform matrix/identity-matrix})

(defn set-transform [camera transform-matrix]
  (assoc camera :transform transform-matrix))

(defn ray-for-pixel [camera x y])

(defn render [camera world])

