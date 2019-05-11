(ns demo.fresnel-demo
  (:require [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.color :as color]
            [raytracer.canvas :as canvas]            
            [raytracer.ray :as ray]
            [raytracer.matrix :as matrix]
            [raytracer.transform :as transform]
            [raytracer.shapes :as shapes]
            [raytracer.materials :as materials]
            [raytracer.light-sources :as light-sources]
            [raytracer.camera :as camera]
            [raytracer.pattern :as pattern]
            [raytracer.perlin :as perlin]
            [raytracer.world :as world]))

;;; Note: the result of this demo changes with the evolution of the underlying functions

(def ^:dynamic *output-file* "fresnel-demo.ppm")
(def ^:dynamic *image-resolution* [800 600])

(def halfπ (/ Math/PI 2))
(def partπ (/ Math/PI 4))

(def wall (-> (shapes/plane)
              (shapes/change-material (materials/material :specular 0.0
                                                          :reflectivity 0.0
                                                          :transparency 0.0
                                                          :pattern (pattern/checker (color/color 1 1 1)
                                                                                    (color/color 0 0 0))))
              (shapes/change-transform (transform/rotate-x (/ Math/PI 2)
                                                           (transform/translate 0 40 0)))))


(def glass-sphere (-> (shapes/sphere)
                      (shapes/change-material (materials/material :color (color/color 0 0 0.002)
                                                                  :shiness 400
                                                                  :specular 0.999
                                                                  :diffuse 0.7
                                                                  :specular 0.3
                                                                  :refractive-index 1.52
                                                                  :reflectivity 0.05
                                                                  :transparency 0.95))                       
                      (shapes/change-transform (transform/translate 0.0 0 0.0))))

(def air-bubble (-> (shapes/sphere)
                    (shapes/change-material (materials/material :diffuse 0.7
                                                                :specular 0.3
                                                                :refractive-index 1.000290
                                                                :reflectivity 0.05
                                                                :transparency 0.95))
                       
                    (shapes/change-transform (transform/translate 0.0 0 0.0))))


(def world (-> (world/create)
               (world/set-light-sources (light-sources/create-point-light (point/point -100 100 -100)
                                                                          (color/color 2 2 2)))
               (world/set-objects [wall glass-sphere air-bubble])
               (update :material #(materials/update-material % :color (color/color 0.1 0.1 0.3)))))

(defn create-camera [width height]
  (camera/set-transform (camera/camera width height (/ Math/PI 3))
                        (world/view-transform (point/point 0 0 -4)
                                              (point/point 0 0 0)
                                              (svector/svector 0 1 0))))

(defn render-demo
  ([] (apply render-demo *image-resolution*))
  ([width height]
   (spit *output-file*
         (canvas/canvas-to-ppm (camera/render (create-camera width height)
                                              world)))))
