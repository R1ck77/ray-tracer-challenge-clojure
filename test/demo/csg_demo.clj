(ns demo.csg-demo
  (:require [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.world :as world]
            [raytracer.camera :as camera]
            [raytracer.canvas :as canvas]))

(def ^:dynamic *output-file* "csg-demo.ppm")
(def ^:dynamic *image-resolution* [800 600])

(defn- create-world []
  (world/world))

(defn- create-camera [world width height]
  (camera/camera width height (/ Math/PI 3)))

(defn- render-world [width height]
  (let [world (create-world)]
    (camera/render (create-camera world width height)
                   world)))

(defn render-demo
  ([] (apply render-demo *image-resolution*))
  ([width height]
   (spit *output-file*
         (canvas/canvas-to-ppm
          (render-world width height)))))
