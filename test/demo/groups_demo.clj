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

(def halfπ (/ Math/PI 2))
(def partπ (/ Math/PI 4))

(def room-material (material/material :specular 0.0
                                      :reflectivity 0.5
                                      :pattern (pattern/change-transform (pattern/checker (color/color 1 0 0)
                                                                                          (color/color 0 0 1))
                                                                         matrix/identity-matrix)))

#_(def floor (-> (shapes/plane)
               (shapes/change-material room-material)
               (shapes/change-transform (transform/translate 0 -0.0001 0))))

(defn- create-marble [x z]
  (-> (shapes/sphere)
      (shapes/change-material
       (material/material :diffuse 0.1
                          :specular 0.8
                          :reflectivity 0.5
                          :transparency 0.99
                          :refractive-index 0.8
                          :color (color/color 0 1 0)))
      (shapes/change-transform
       (transform/translate (* 1.0 x) 1.0 (* 1.0 z)
                            (transform/scale 0.5 0.5 0.5)))))

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
                 (apply shapes/group (vals marbles-dict))
                 (let [{:keys [min-x max-x min-z max-z]} (get-group-extremes marbles-dict)
                       half-x (/ (+ max-x min-x) 2)
                       half-z (/ (+ max-z min-z) 2)
                       partitioned (bin-marbles marbles-dict half-x half-z)]
                   (apply shapes/group
                          (map #(partition-marbles (into {} %) limit) (vals partitioned)))))]
    (print-volume result)))

(defn- create-all-marbles [size]
  {[0 0] (create-marble 0 0)}
  #_(into {}
   (for [x (range (- size) size)
         z (range (- size) size)]
     (vector [x z] (create-marble x z)))))

(defn- create-marble-floor []
  (shapes/change-transform
   (time (partition-marbles (create-all-marbles 3) *maximum-group-size*))
      (transform/translate 0 0.128 0)))

(def world (-> (world/world [(create-marble-floor) #_ floor])
               (world/set-light-sources (light-sources/create-point-light (point/point -10 10 -10)
                                                                          (color/color 1 1 1)))
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

(defn quick-demo []
  ;;; 270" no smart grouping, 20x20
  ;;; 228" with just one extra group? Not sure
  (with-redefs [*image-resolution* [300 300]
                group/*statistics* true
                world/*maximum-reflections* 0]
    (reset! group/hit-count-statistics [0 0])
    (time (render-demo))
    (println @group/hit-count-statistics)))
