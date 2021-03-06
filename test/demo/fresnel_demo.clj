(ns demo.fresnel-demo
  (:require [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.color :as color]
            [raytracer.canvas :as canvas]            
            [raytracer.ray :as ray]
            [raytracer.matrix :as matrix]
            [raytracer.transform :as transform]
            [raytracer.shapes :as shapes]
            [raytracer.shapes.shared :as shared]
            [raytracer.material :as material]
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
              (shared/change-material (material/material :specular 0.0
                                                         :reflectivity 0.0
                                                         :transparency 0.0
                                                         :pattern (pattern/checker (color/color 1 1 1)
                                                                                   (color/color 0 0 0))))
              (shared/change-transform (transform/rotate-x (/ Math/PI 2)
                                                    (transform/translate 0 40 0)))))


(def glass-sphere (-> (shapes/sphere)
                      (shared/change-material (material/material :color (color/color 0 0 0.002)
                                                                 :shiness 400
                                                                 :specular 0.999
                                                                 :diffuse 0.1
                                                                 :specular 0.3
                                                                 :refractive-index 1.52
                                                                 :reflectivity 0.05
                                                                 :transparency 0.95))                       
                      (shared/change-transform (transform/translate 0.0 0 0.0))))

(def air-bubble (-> (shapes/sphere)
                    (shared/change-material (material/material :diffuse 0.01
                                                               :specular 0.3
                                                               :refractive-index 1.000290
                                                               :reflectivity 0.1
                                                               :transparency 0.99))
                    
                    (shared/change-transform (transform/scale 0.98 0.98 0.98))))


(def world (-> (world/world)
               (world/set-light-sources (light-sources/create-point-light (point/point -100 100 -100)
                                                                          (color/color 2 2 2)))
               (world/set-objects [wall glass-sphere air-bubble])
               (update :material #(material/update-material % :color (color/color 0.1 0.1 0.3)))))

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
