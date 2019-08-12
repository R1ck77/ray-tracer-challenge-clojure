(ns demo.obj-demo
  (:require [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.tuple :as tuple]
            [raytracer.canvas :as canvas]            
            [raytracer.color :as color]
            [raytracer.ray :as ray]
            [raytracer.matrix :as matrix]
            [raytracer.transform :as transform]
            [raytracer.shapes :as shapes]
            [raytracer.shapes.shared :as shared]
            [raytracer.shapes.group :as group]
            [raytracer.material :as material]
            [raytracer.pattern :as pattern]
            [raytracer.shapes.obj :as obj]
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
               (shared/transform (transform/translate 0 -0.00001 0))))

(defn create-camera [width height]
  (camera/set-transform (camera/camera width height (/ Math/PI 3))
                        (world/view-transform (point/point 0 2.0 -5)
                                              (point/point 0 0 0)
                                              (svector/svector 0 1 0))))

(defn load-wavefront []
  (-> (obj/obj (clojure.java.io/resource "obj/teapot.obj"))
      (shapes/change-material (material/material :specular 0.4
                                                 :reflectivity 0.4
                                                 :ambient 0.3
                                                 :diffuse 0.5
                                                 :pattern (pattern/solid (color/color 1.0 0.0 0.0))))))

(defn render-demo
  ([] (apply render-demo *image-resolution*))
  ([width height]
   (let [world (-> (world/world [(load-wavefront) floor])
                   (world/set-light-sources (light-sources/create-point-light (point/point -10 10 -10)
                                                                              (color/color 1 1 1)))
                   (update :material #(material/update-material % :color (color/color 0.0 0.0 0.0))))]
     (println "* Rendering…")
     (spit *output-file*
           (canvas/canvas-to-ppm (camera/render (create-camera width height)
                                                world))))))

(defn quick-demo []
  ;;; 270" no smart grouping, 20x20
  ;;; 228" with just one extra group? Not sure
  (with-redefs [*image-resolution* [300 300]
                group/*statistics* true
                world/*maximum-reflections* 0]
    (reset! group/hit-count-statistics [0 0])
    (time (render-demo))
    (println @group/hit-count-statistics)))
