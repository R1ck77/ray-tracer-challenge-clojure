(ns demo.csg-demo
  (:require [raytracer.point :as point]
            [raytracer.const :as const]
            [raytracer.svector :as svector]
            [raytracer.color :as color]
            [raytracer.world :as world]
            [raytracer.camera :as camera]
            [raytracer.canvas :as canvas]
            [raytracer.material :as material]
            [raytracer.pattern :as pattern]
            [raytracer.shapes :as shapes]
            [raytracer.transform :as transform]
            [raytracer.matrix :as matrix]
            [raytracer.light-sources :as light-sources]
            [raytracer.shapes.shared :as shared]
            [raytracer.shapes.placement :as placement]))

(def ^:dynamic *output-file* "csg-demo.ppm")
(def ^:dynamic *image-resolution* [800 600])

(def platform-size 5)

(defn create-wall-material [color-a color-b]
  (material/material :specular 0.3
                     :reflectivity 0.01
                     :transparency 0.0
                     :ambient 0.1
                     :diffuse 0.6
                     :pattern (pattern/change-transform (pattern/checker color-a color-b)
                                                        (transform/scale 0.25 0.25 0.25))))


(def walls-material (create-wall-material (color/color 0 0.8 0)
                                          (color/color 1 1 1)))

(defn- create-wall []
  (-> (shapes/plane)
      (shared/change-transform (transform/translate 0 0 5 (transform/rotate-x const/half𝛑)))
      (shared/change-material walls-material)))

(def floor-material (create-wall-material (color/color 0.8 0 0)
                                          (color/color 1 1 1)))

(defn- create-floor []
  (-> (shapes/plane)
      (shared/change-transform (transform/rotate-x 0))
      (shared/change-material floor-material)))


(def lens-material (material/material :specular 0.8
                                      :reflectivity 0.001
                                      :ambient 0.05
                                      :diffuse 0.05
                                      :transparency 0.9999
                                      :refractive-index 1.01
                                      :pattern (pattern/solid (color/color 1 1 1))))

(defn- create-lens2 []
  (-> (shapes/csg :intersection
                  (shared/change-material (shared/change-transform (shapes/sphere)
                                                                   (transform/translate -0.5 0 0)) lens-material)
                  (shared/change-material (shared/change-transform (shapes/sphere)
                                                                   (transform/translate 0.5 0 0)) lens-material))
      (shared/change-transform (transform/translate 0 2.0 0
                                                    (transform/rotate-y const/half𝛑)))))

(defn- create-lens1 []
  (-> (shapes/csg :intersection
                  (shared/change-material (shared/change-transform (shapes/sphere)
                                                                   (transform/translate 0 2 -0.5)) lens-material)
                  (shared/change-material (shared/change-transform (shapes/sphere)
                                                                   (transform/translate 0 2 0.5)) lens-material))))

(defn- create-world []
  (assoc (world/set-light-sources (world/world [(create-lens2)
                                          (create-floor)
                                          (create-wall)])
                            (light-sources/create-point-light (point/point 0 100 -100)
                                                              (color/color 1 1 1)))
         :sky-material (material/material :specular 0
                                          :reflectivity 0
                                          :transparency 0
                                          :pattern (pattern/solid (color/color 1 0 0)))))

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
