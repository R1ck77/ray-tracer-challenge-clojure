(ns demo.phong-scene
  (:require [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.canvas :as canvas]            
            [raytracer.ray :as ray]
            [raytracer.transform :as transform]
            [raytracer.materials :as materials]
            [raytracer.light-sources :as light-sources]
            [raytracer.camera :as camera]
            [raytracer.world :as world]))

(def width 100)
(def height 50)

(def halfπ (/ Math/PI 2))
(def partπ (/ Math/PI 4))

(def room-material (materials/material :specular 0
                                       :color [1 0.9 0.9]))

(def floor (-> (ray/sphere)
               (ray/change-material room-material)
               (ray/change-transform (transform/scale 10, 0.01 10))))

(def left-wall (-> (ray/sphere)
                   (ray/change-material room-material)
                   (ray/change-transform (->> (transform/scale 10 0.01 10)
                                              (transform/rotate-x halfπ)
                                              (transform/rotate-y (- partπ))
                                              (transform/translate 0 0 5)))))

(def right-wall (-> (ray/sphere)
                    (ray/change-material room-material)
                    (ray/change-transform (->> (transform/scale 10 0.01 10)
                                               (transform/rotate-x halfπ)
                                               (transform/rotate-y partπ)
                                               (transform/translate 0 0 5)))))

(def left-sphere (-> (ray/sphere)
                     (ray/change-material (materials/material :color [1 0.8 0.1], :diffuse 0.7, :specular 0.3))
                     (ray/change-transform (->> (transform/scale 0.33 0.33 0.33)
                                                (transform/translate -1.5 0.33 -0.75)))))

(def middle-sphere (-> (ray/sphere)
                       (ray/change-material (materials/material :color [0.1 1 0.5], :diffuse 0.7, :specular 0.3))
                       (ray/change-transform (transform/translate -0.5 1 0.5))))

(def right-sphere (-> (ray/sphere)
                      (ray/change-material (materials/material :color [0.5 1 0.1], :diffuse 0.7, :specular 0.3))
                      (ray/change-transform (->> (transform/scale 0.5 0.5 0.5)
                                                 (transform/translate 1.5 0.5 -0.5)))))


(def world (world/set-light-sources (world/set-objects (world/create) [floor left-wall right-wall
                                                                       left-sphere middle-sphere right-sphere])
                                    (light-sources/create-point-light (point/point -10 10 -10)
                                                                      [1 1 1])))

(def camera (camera/set-transform (camera/camera width height (/ Math/PI 3))
                                  (world/view-transform (point/point 0 1.5 -5)
                                                        (point/point 0 1 0)
                                                        (svector/svector 0 1 0))))

(defn render-demo []
  (spit "output.ppm"
        (canvas/canvas-to-ppm (camera/render camera world))))
