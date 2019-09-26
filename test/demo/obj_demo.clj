(ns demo.obj-demo
  (:require [clojure.java.io :as io]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.tuple :as tuple]
            [raytracer.canvas :as canvas]            
            [raytracer.color :as color]
            [raytracer.ray :as ray]
            [raytracer.matrix :as matrix]
            [raytracer.transform :as transform]
            [raytracer.shapes :as shapes]
            [raytracer.shapes.triangle :as triangle]
            [raytracer.shapes.shared :as shared]
            [raytracer.shapes.group :as group]
            [raytracer.material :as material]
            [raytracer.pattern :as pattern]
            [raytracer.shapes.obj :as obj]
            [raytracer.light-sources :as light-sources]
            [raytracer.camera :as camera]
            [raytracer.world :as world]))

;;; Note: the result of this demo changes with the evolution of the underlying functions

(def ^:dynamic *output-file* "obj-demo.ppm")
(def ^:dynamic *image-resolution* [800 600])
(def ^:dynamic *wavefront-model* :pyramid) ; :astronaut, :teapot or :pyramid

(def halfπ (/ Math/PI 2))
(def partπ (/ Math/PI 4))

(def room-material (material/material :specular 0.0
                                      :reflectivity 0.5
                                      :pattern (pattern/change-transform (pattern/checker (color/color 1 0 0)
                                                                                          (color/color 0 0 1))
                                                                         matrix/identity-matrix)))

(def floor (-> (shapes/plane)
               (shared/change-material room-material)
               (shared/change-transform (transform/translate 0 -0.00001 0))))

(defn- load-astronaut []
  (let [model (-> "astronaut1/astronaut1.obj.gz"
                  io/resource
                  io/input-stream
                  java.util.zip.GZIPInputStream.
                  obj/obj)]
    (println "Astronaut model loaded…")
    {:wavefront (shared/change-material (shared/change-transform model (transform/translate -1/4 1/2 0
                                                                     (transform/scale 0.1 0.1 0.1
                                                                                      (transform/rotate-y (/ Math/PI 2)))))
                                        (material/material :specular 0.1
                                                           :ambient 0.3
                                                           :diffuse 0.6
                                                           :reflectivity 0.03
                                                           :transparency 0.0
                                                           :pattern (pattern/solid (color/color 0.99 0.99 1.0))))
     :transform (world/view-transform (point/point 0 1.0 -2)
                                      (point/point 0 0.5 0)
                                      (svector/svector 0 1 0))}))

(defn- load-teapot []
  (let [model (obj/obj (clojure.java.io/resource "obj/teapot.obj"))]
    (println "Teapot model loaded…")
    {:wavefront model
     :transform (world/view-transform (point/point 0.2 1.5 -6)
                                      (point/point 0 0.8 0)
                                      (svector/svector 0 1 0))}))

(defn- load-pyramid []
  (let [model (obj/obj (clojure.java.io/resource "pyramid/pyramid.obj"))]
    (println "pyramid model loaded…")
    {:wavefront (shared/change-transform (shapes/group [(shapes/sphere)])
                                         (transform/translate 1.5 1 3)),
     :transform (world/view-transform (point/point 0.35 1.27 -4)
                                      (point/point 0 0.5 0)
                                      (svector/svector 0 1 0))}))

(defn- load-wavefront []
  (case *wavefront-model*
    :astronaut (load-astronaut)
    :teapot (load-teapot)
    :pyramid (load-pyramid)))

(defn create-camera [width height transform]
  (camera/set-transform (camera/camera width height (/ Math/PI 3))
                        transform))

(defn render-demo
  ([] (apply render-demo *image-resolution*))
  ([width height]
   (let [{wavefront :wavefront, transform :transform} (load-wavefront)
         world (-> (world/world [wavefront floor])
                   (world/set-light-sources (light-sources/create-point-light (point/point -10 10 -10)
                                                                              (color/color 1 1 1)))
                   (update :material #(material/update-material % :color (color/color 0.0 0.0 0.0))))]
     (println "* Rendering…")
     (time (spit *output-file*
                 (canvas/canvas-to-ppm (camera/render (create-camera width height transform)
                                                      world)))))))

(defn quick-demo []
  ;;; 270" no smart grouping, 20x20
  ;;; 228" with just one extra group? Not sure
  (with-redefs [*image-resolution* [320 200]
                ray/*statistics* true
                world/*maximum-reflections* 0]
    (reset! ray/hit-count-statistics [0 0])
    (time (render-demo))
    (println @ray/hit-count-statistics)))
