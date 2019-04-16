(ns raytracer.camera)

(defn camera [h-size v-size field-of-view]
  {:h-size h-size
   :v-size v-size
   :fov field-of-view})

(defn set-transform [transform-matrix])

(defn ray-for-pixel [camera x y])

(defn render [camera world])

