(ns demo.plane-demo
  (:require [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.color :as color]
            [raytracer.canvas :as canvas]            
            [raytracer.ray :as ray]
            [raytracer.transform :as transform]
            [raytracer.shapes :as shapes]
            [raytracer.material :as material]
            [raytracer.light-sources :as light-sources]
            [raytracer.camera :as camera]
            [raytracer.world :as world]))

;;; Note: the result of this demo changes with the evolution of the underlying functions

(def halfπ (/ Math/PI 2))
(def partπ (/ Math/PI 4))

(def room-material (material/material :specular 0.0
                                       :color (color/color 1 0.9 0.9)))

(def floor (-> (shapes/plane)
               (shapes/change-material room-material)))

(def left-sphere (-> (shapes/sphere)
                     (shapes/change-material (material/material :color (color/color 1 0.8 0.1), :diffuse 0.7, :specular 0.3))
                     (shapes/change-transform (->> (transform/scale 0.33 0.33 0.33)
                                                (transform/translate -1.5 0.33 -0.75)))))

(def middle-sphere (-> (shapes/sphere)
                       (shapes/change-material (material/material :color (color/color 0.1 1 0.5), :diffuse 0.7, :specular 0.3))
                       (shapes/change-transform (transform/translate -0.5 1 0.5))))

(def right-sphere (-> (shapes/sphere)
                      (shapes/change-material (material/material :color (color/color 0.5 1 0.1), :diffuse 0.7, :specular 0.3))
                      (shapes/change-transform (->> (transform/scale 0.5 0.5 0.5)
                                                 (transform/translate 1.5 0.5 -0.5)))))

(def world (world/set-light-sources (world/set-objects (world/create) [floor left-sphere middle-sphere right-sphere])
                                    (light-sources/create-point-light (point/point -10 10 -10)
                                                                      (color/color 1 1 1))))

(defn create-camera [width height]
  (camera/set-transform (camera/camera width height (/ Math/PI 3))
                        (world/view-transform (point/point 0 1.5 -5)
                                              (point/point 0 1 0)
                                              (svector/svector 0 1 0))))

(defn render-demo
  ([] (render-demo 400 200))
  ([width height]
   (spit "plane-demo.ppm"
         (canvas/canvas-to-ppm (camera/render (create-camera width height)
                                              world)))))
