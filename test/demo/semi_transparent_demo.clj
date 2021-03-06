(ns demo.semi-transparent-demo
  (:require [raytracer.point :as point]
            [raytracer.svector :as svector]
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


(def ^:dynamic *output-file* "pond-demo.ppm")
(def ^:dynamic *image-resolution* [800 600])

(def halfπ (/ Math/PI 2))
(def partπ (/ Math/PI 4))


;;; Set the y to 5 and stranger things start happening
(def water (-> (shapes/plane)
               (shared/change-material (material/material :color [0 0 0.01]
                                                          :specular 1
                                                          :shiness 300
                                                          :reflectivity 0.8
                                                          :transparency 0.9 
                                                          :pattern nil))
               (shared/change-transform (transform/translate 0 5 0))))

(def pool-bottom (-> (shapes/plane)
                     (shared/change-material (material/material :specular 0.1
                                                                :ambient 0.05
                                                                :reflectivity 0.0
                                                                :transparency 0.0
                                                                :pattern (pattern/checker [0 8 8] [0.01 0.01 0.01])))
                     (shared/change-transform (transform/translate 0 1e-3 0))))


(def matte-sphere (-> (shapes/sphere)
                      (shared/change-material (material/material :color [0.9 0 0]
                                                                 :shiness 100
                                                                 :specular 0.8
                                                                 :diffuse 0.7
                                                                 :specular 0.1
                                                                 :refractive-index 1.52
                                                                 :reflectivity 0.1
                                                                 :transparency 0.01))                       
                      (shared/change-transform (transform/translate -4 2 0 (transform/scale 4 4 4)))))

(def world (-> (world/world)
               (world/set-light-sources (light-sources/create-point-light (point/point -10 10 -10)
                                                                          [1 1 1]))
               (world/set-objects [water pool-bottom matte-sphere])
               (update :material #(material/update-material % :color [0.1 0.1 0.3]))))

(defn create-camera [width height]
  (camera/set-transform (camera/camera width height (/ Math/PI 3))
                        (world/view-transform (point/point 0 11 -10)
                                              (point/point 0 0 0)
                                              (svector/svector 0 1 0))))

(defn render-demo
  ([] (apply render-demo *image-resolution*))
  ([width height]
   (spit *output-file*
         (canvas/canvas-to-ppm (camera/render (create-camera width height)
                                              world)))))
