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
            [raytracer.light-sources :as light-sources]
            [raytracer.camera :as camera]
            [raytracer.pattern :as pattern]
            [raytracer.perlin :as perlin]
            [raytracer.world :as world]))

;;; Note: the result of this demo changes with the evolution of the underlying functions

(def ^:dynamic *output-file* "reflection-demo.ppm")
(def ^:dynamic *image-resolution* [800 600])

(def halfπ (/ Math/PI 2))
(def partπ (/ Math/PI 4))

(def perlin-data (perlin/create-perlin-data [3 3 3]))

(def room-material (material/material :specular 0.0
                                      :reflectivity 0.5
                                      :pattern (pattern/change-transform (pattern/checker (color/color 1 0 0)
                                                                                          (color/color 0 0 1))
                                                                         matrix/identity-matrix)))

(def floor (-> (shapes/plane)
               (shapes/change-material room-material)
               (shapes/change-transform (transform/translate 0 -0.0001 0))))

(def left-sphere (-> (shapes/sphere)
                     (shapes/change-material (material/material :diffuse 0.7, :specular 0.3
                                                                :reflectivity 0.3
                                                                :pattern (pattern/change-transform (pattern/stripe (color/color 0 1 0)
                                                                                                                   (color/color 1 1 1))
                                                                                                   (transform/scale 0.2 0.2 0.2
                                                                                                                    (transform/rotate-y (/ Math/PI 5))))))
                     (shapes/change-transform (->> (transform/scale 0.33 0.33 0.33)
                                                   (transform/translate -1.5 0.33 -0.75)))))

(def middle-sphere (-> (shapes/sphere)
                       (shapes/change-material (material/material :color (color/color 0 0.05 0.1)
                                                                  :diffuse 0.1
                                                                  :specular 0.3
                                                                  :reflectivity 0.1
                                                                  :transparency 0.95
                                                                  :refractive-index 2.0))
                       
                       (shapes/change-transform (transform/translate -0.5 1 0.5))))

(def air-sphere (-> (shapes/sphere)
                    (shapes/change-material (material/material :color (color/color 0 0 0)
                                                               :diffuse 0.0
                                                               :specular 0.0
                                                               :reflectivity 1.0
                                                               :transparency 1.0
                                                               :refractive-index 1.0))
                    
                    (shapes/change-transform (transform/translate -0.5 1 0.5
                                                                  (transform/scale 0.5 0.5 0.5)))))

(def back-sphere (-> (shapes/sphere)
                     (shapes/change-material (material/material :color (apply color/color (map #(/ % 255) [200 110 200]))
                                                                :diffuse 0.4
                                                                :specular 0.5
                                                                :refractive-index 2.0
                                                                :reflectivity 0.3
                                                                :transparency 0.0
                                                                :refractive-index 0.0))
                     
                     (shapes/change-transform (transform/translate 1.75 2 5.5
                                                                   (transform/scale 2 2 2)))))

(def right-sphere (-> (shapes/sphere)
                      (shapes/change-material (material/material :diffuse 0.7
                                                                 :specular 0.3
                                                                 :reflectivity 0.2
                                                                 :pattern (pattern/change-transform (pattern/perturb-pattern (pattern/ring (color/color 1 1 1)
                                                                                                                                           (color/color 0.0 0 0.0)) 
                                                                                                                             (fn [pattern point]
                                                                                                                               (let [noise (perlin/noise perlin-data (vector (:x point)
                                                                                                                                                                             (:y point)
                                                                                                                                                                             (:z point)))]
                                                                                                                                 (pattern/color-at pattern (point/point (+ (:x point) noise)
                                                                                                                                                                        (:y point)
                                                                                                                                                                        (+ (:z point) noise))))))
                                                                                                    (transform/scale 0.5 0.125 0.125
                                                                                                                     (transform/rotate-z 0.23423)))))
                      (shapes/change-transform (->> (transform/scale 0.5 0.5 0.5)
                                                    (transform/translate 1.5 0.5 -0.5)))))

(def world (-> (world/world)
               (world/set-light-sources (light-sources/create-point-light (point/point -10 10 -10)
                                                                          (color/color 1 1 1)))
               (world/set-objects [floor left-sphere middle-sphere air-sphere back-sphere right-sphere])
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
