(ns demo.groups-demo
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
            [raytracer.shapes.bounding-box :as bounding-box]
            [raytracer.material :as material]
            [raytracer.pattern :as pattern]
            [raytracer.light-sources :as light-sources]
            [raytracer.camera :as camera]
            [raytracer.world :as world]))

;;; Note: the result of this demo changes with the evolution of the underlying functions

(def ^:dynamic *output-file* "groups-demo.ppm")
(def ^:dynamic *image-resolution* [800 600])
(def ^:dynamic *maximum-group-size* 4)
(def ^:dynamic *print-aabb-volumes* false)
(def ^:dynamic *marbles-side* 100)

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

(defn- create-marble [x z]
  (let [space 0.5
        material (material/material :diffuse (rand 0.6)
                                    :specular (rand 0.9)
                                    :reflectivity (rand 0.5)
                                    :transparency (rand 0.99)
                                    :refractive-index (rand 1.0)
                                    :pattern (pattern/solid
                                              (color/color (rand 1)
                                                           (rand 1)
                                                           (rand 1))))
        shift-function #(+ (* % 0.5) (* space (- % (rand))))
        x-shift (shift-function x)
        z-shift (shift-function z)
        scale-f (/ 1 4)
        transform (transform/translate x-shift 0.25 z-shift
                                       (transform/scale scale-f scale-f scale-f))
        inverse-transform (transform/scale (/ scale-f) (/ scale-f) (/ scale-f)
                                   (transform/translate (- x-shift) -0.25 (- z-shift)))]
    (shapes/sphere :material material
                   :transform transform
                   :inverse-transform inverse-transform)))

(defn- get-group-extremes [marbles-dict]
  (let [indices (map first marbles-dict)
        all-x (sort (map first indices))
        all-z (sort (map second indices))]
    {:min-x (first all-x)
     :max-x (last all-x)
     :min-z (first all-z)
     :max-z (last all-z)}))

(defn- bin-marbles [marbles-dict half-x half-z]
  (reduce (fn bin-marble [accumulator [[x z] marble :as indexed-marble]]
            (let [index (+ (if (> x half-x) 1 0)
                           (if (> z half-z) 2 0))]
              (update-in accumulator [index] #(conj % indexed-marble))))
          {0 [], 1 [], 2 [], 3 []}
          marbles-dict))

(defn print-volume [group]
  (when *print-aabb-volumes*
    (let [extremes (bounding-box/get-corners group)
          sides (tuple/sub (second extremes) (first extremes))
          volume (tuple/dot sides sides)]
      (println volume)))
  group)

(defn- partition-marbles [marbles-dict limit]
  (:pre [(> limit 1)])
  (let [result (if (<= (count marbles-dict) limit)
                 (shapes/group (vals marbles-dict))
                 (let [{:keys [min-x max-x min-z max-z]} (get-group-extremes marbles-dict)
                       half-x (/ (+ max-x min-x) 2)
                       half-z (/ (+ max-z min-z) 2)
                       partitioned (bin-marbles marbles-dict half-x half-z)]
                   (shapes/group (map #(partition-marbles (into {} %) limit) (vals partitioned)))))]
    (print-volume result)))

(defn- create-all-marbles [size]
  (into {}
        (pmap (fn [[x z :as coords]]
                (vector coords (create-marble x z)))
              (for [x (range (- size) size)
                    z (range -5 size)]
                (vector x z)))))

(defn- create-marble-floor []
  (shared/transform
   (time (partition-marbles (create-all-marbles *marbles-side*) *maximum-group-size*))
      (transform/translate 0 0 0)))

(defn create-camera [width height]
  (camera/set-transform (camera/camera width height (/ Math/PI 3))
                        (world/view-transform (point/point 0 2.5 -5)
                                              (point/point 0 1 0)
                                              (svector/svector 0 1 0))))

(defn render-demo
  ([] (apply render-demo *image-resolution*))
  ([width height]
   (println "* Distributing/grouping the marbles…")
   (let [world (-> (world/world [(create-marble-floor) floor])
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
                world/*maximum-reflections* 0
                *marbles-side* 20]
    (reset! group/hit-count-statistics [0 0])
    (time (render-demo))
    (println @group/hit-count-statistics)))
