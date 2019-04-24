(ns demo.pattern-demo
  (:require [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.canvas :as canvas]            
            [raytracer.ray :as ray]
            [raytracer.matrix :as matrix]
            [raytracer.transform :as transform]
            [raytracer.shapes :as shapes]
            [raytracer.materials :as materials]
            [raytracer.light-sources :as light-sources]
            [raytracer.camera :as camera]
            [raytracer.pattern :as pattern]
            [raytracer.world :as world]))

;;; Note: the result of this demo changes with the evolution of the underlying functions

(def halfπ (/ Math/PI 2))
(def partπ (/ Math/PI 4))

(def room-material (materials/material :specular 0.0
                                       :pattern (pattern/change-transform (pattern/checker [1 0 0] [0 0 1])
                                                                          matrix/identity-matrix)))

(def floor (-> (shapes/plane)
               (shapes/change-material room-material)))

(def left-sphere (-> (shapes/sphere)
                     (shapes/change-material (materials/material :diffuse 0.7, :specular 0.3
                                                                 :pattern (pattern/change-transform (pattern/stripe [1 0 0] [1 1 1])
                                                                                                    (transform/scale 0.2 0.2 0.2
                                                                                                                     (transform/rotate-y (/ Math/PI 5))))))
                     (shapes/change-transform (->> (transform/scale 0.33 0.33 0.33)
                                                (transform/translate -1.5 0.33 -0.75)))))

(def middle-sphere (-> (shapes/sphere)
                       (shapes/change-material (materials/material :diffuse 0.7
                                                                   :specular 0.3
                                                                   :pattern (pattern/change-transform (pattern/gradient [1 0 0] [0 0 1])
                                                                                                      (transform/rotate-x (/ Math/PI 2)))))
                       (shapes/change-transform (transform/translate -0.5 1 0.5))))

(def right-sphere (-> (shapes/sphere)
                      (shapes/change-material (materials/material :diffuse 0.7
                                                                  :specular 0.3
                                                                  :pattern (pattern/change-transform (pattern/ring [1 1 1] [0.0 0 0.0])
                                                                                                     (transform/scale 0.5 0.125 0.125
                                                                                                                      (transform/rotate-z 0.23423)))))
                      (shapes/change-transform (->> (transform/scale 0.5 0.5 0.5)
                                                 (transform/translate 1.5 0.5 -0.5)))))

(def world (world/set-light-sources (world/set-objects (world/create) [floor left-sphere middle-sphere right-sphere])
                                    (light-sources/create-point-light (point/point -10 10 -10)
                                                                      [1 1 1])))

(defn create-camera [width height]
  (camera/set-transform (camera/camera width height (/ Math/PI 3))
                        (world/view-transform (point/point 0 1.5 -5)
                                              (point/point 0 1 0)
                                              (svector/svector 0 1 0))))

(defn render-demo
  ([] (render-demo 400 200))
  ([width height]
   (spit "pattern-demo.ppm"
         (canvas/canvas-to-ppm (camera/render (create-camera width height)
                                              world)))))
