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

(def floor (-> (ray/sphere)
               (ray/change-material (materials/material :specular 0
                                                        :color [1 0.9 0.9]))
               (ray/change-transform (transform/scale 10, 0.01 10))))

(def world (world/set-light-sources (world/set-objects (world/create) [floor])
                                    (light-sources/create-point-light (point/point -10 10 -10)
                                                                      [1 1 1])))

(def camera (camera/set-transform (camera/camera 100 50 (/ Math/PI 3))
                                  (world/view-transform (point/point 0 1.5 -5)
                                                        (point/point 0 1 0)
                                                        (svector/svector 0 1 0))))

(defn render-demo []
  (spit "output.ppm"
        (canvas/canvas-to-ppm (camera/render camera world))))
