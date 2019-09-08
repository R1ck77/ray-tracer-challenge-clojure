(ns demo.csg-demo
  (:require [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.world :as world]
            [raytracer.camera :as camera]
            [raytracer.canvas :as canvas]
            [raytracer.shapes :as shapes]
            [raytracer.transform :as transform]
            [raytracer.shapes.shared :as shared]
            [raytracer.shapes.placement :as placement]))

(def ^:dynamic *output-file* "csg-demo.ppm")
(def ^:dynamic *image-resolution* [800 600])

(defn- create-floor []
  (shapes/csg :intersection
              (shared/change-transform (shapes/sphere)
                                       (transform/scale 100 100 100))
              (shared/change-transform (shapes/cube)
                                       (transform/scale 100 0.01 100))))

(defn- create-world []
  (world/world [(create-floor)]))

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
