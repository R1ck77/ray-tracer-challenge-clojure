(ns demo.reflection-demo
  (:require [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.canvas :as canvas]            
            [raytracer.color :as color]
            [raytracer.ray :as ray]
            [raytracer.matrix :as matrix]
            [raytracer.transform :as transform]
            [raytracer.shapes :as shapes]
            [raytracer.material :as material]
            [raytracer.pattern :as pattern]
            [raytracer.light-sources :as light-sources]
            [raytracer.camera :as camera]
            [raytracer.world :as world]))

;;; Note: the result of this demo changes with the evolution of the underlying functions

(def ^:dynamic *output-file* "groups-demo.ppm")
(def ^:dynamic *image-resolution* [800 600])

(def halfπ (/ Math/PI 2))
(def partπ (/ Math/PI 4))

(def room-material (material/material :specular 0.0
                                      :reflectivity 0.5
                                      :pattern (pattern/change-transform (pattern/checker (color/color 1 0 0)
                                                                                          (color/color 0 0 1))
                                                                         matrix/identity-matrix)))

(def floor (-> (shapes/plane)
               (shapes/change-material room-material)
               (shapes/change-transform (transform/translate 0 -0.0001 0))))

(defn- create-marble [x z]
  (-> (shapes/sphere)
      (shapes/change-material
       (material/material :diffuse 0.1
                          :specular 0.8
                          :reflectivity 0.5
                          :transparency 0.99
                          :refractive-index 0.8
                          :color (color/color 0 1 0)))
      (shapes/change-transform
       (transform/translate (* 0.6 x) 0.0 (* 0.6 z)
                            (transform/scale 0.25 0.25 0.25)))))

(defn- create-marble-floor []
  (shapes/change-transform
   (shapes/group
    (vec
     (for [x (range -2 2)
           z (range -2 2)]
       (create-marble x z))))
   (transform/translate 0 0.128 0)))

(def world (-> (world/world [(create-marble-floor) floor])
               (world/set-light-sources (light-sources/create-point-light (point/point -10 10 -10)
                                                                          (color/color 1 1 1)))
               (update :material #(material/update-material % :color (color/color 0.0 0.0 0.0)))))

(defn create-camera [width height]
  (camera/set-transform (camera/camera width height (/ Math/PI 3))
                        (world/view-transform (point/point 0 1.5 -5)
                                              (point/point 0 1 0)
                                              (svector/svector 0 1 0))))

(defn render-demo
  ([] (apply render-demo *image-resolution*))
  ([width height]
   (spit *output-file*
         (canvas/canvas-to-ppm (camera/render (create-camera width height)
                                              world)))))
