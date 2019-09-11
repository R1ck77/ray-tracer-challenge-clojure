(ns demo.csg-demo
  (:require [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.color :as color]
            [raytracer.world :as world]
            [raytracer.camera :as camera]
            [raytracer.canvas :as canvas]
            [raytracer.shapes :as shapes]
            [raytracer.transform :as transform]
            [raytracer.light-sources :as light-sources]
            [raytracer.shapes.shared :as shared]
            [raytracer.shapes.placement :as placement]))

(def ^:dynamic *output-file* "csg-demo.ppm")
(def ^:dynamic *image-resolution* [800 600])

(def platform-size 20)

(defn- create-floor []
  #_(shapes/csg :intersection
              (shared/change-transform (shapes/sphere)
                                       (transform/scale platform-size
                                                        platform-size
                                                        platform-size))
              (shared/change-transform (shapes/cube)
                                       (transform/scale platform-size
                                                        0.01
                                                        platform-size)))
  (shapes/csg :difference
              (shapes/cube)
              (shapes/sphere)))

(defn- create-world []
  (world/set-light-sources (world/world [(shapes/sphere) (create-floor)])
                           (light-sources/create-point-light (point/point 0 10 -10)
                                                             (color/color 1 1 1))))

(defn- create-camera [width height]
  (camera/set-transform (camera/camera width height (/ Math/PI 3))
                        (world/view-transform (point/point 0 3 -5)
                                              (point/point 0 1 0)
                                              (svector/svector 0 1 0))))

(defn- render-world [width height]
  (let [world (create-world)]
    (camera/render (create-camera width height)
                   world)))

(defn render-demo
  ([] (apply render-demo *image-resolution*))
  ([width height]
   (spit *output-file*
         (canvas/canvas-to-ppm
          (render-world width height)))))
