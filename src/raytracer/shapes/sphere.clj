(ns raytracer.shapes.sphere
  (:require [raytracer.tuple :as tuple]
            [raytracer.svector :as svector]
            [raytracer.point :as point]
            [raytracer.matrix :as matrix]
            [raytracer.shapes.shared :as shared]
            [raytracer.materials :as materials]
            [raytracer.intersection :as intersection]))

(set! *unchecked-math* true)

(defn- ray-sphere-discriminant [this-sphere ray]
  (let [sphere-to-ray (tuple/sub (:origin ray)
                                 (:center this-sphere))
        a (.dot (:direction ray)
                (:direction ray))
        b (* 2 (.dot (:direction ray)
                     sphere-to-ray))
        c (- (.dot sphere-to-ray sphere-to-ray) 1)]
    [a b c (- (* b b ) (* 4 a c))]))

(defn- intersect-sphere-space [this-sphere ray-in-sphere-space]
  (let [[a b c discriminant]
        (ray-sphere-discriminant this-sphere ray-in-sphere-space)]
    (if (< discriminant 0)
      []
      [(intersection/intersection (/ (- (- b) (Math/sqrt discriminant))
                                     (* 2 a))
                                  this-sphere)
       (intersection/intersection (/ (+ (- b) (Math/sqrt discriminant))
                                     (* 2 a))
                                  this-sphere)])))

(defn compute-normal [shape point]
  (.normalize
   (shared/as-vector
    (matrix/transform (matrix/transpose (:inverse-transform shape))
                      (.sub (matrix/transform (:inverse-transform shape) point)
                            (point/point 0 0 0))))))

(defn sphere []
  {:center (point/point 0 0 0)
   :radius 1.0
   :material (materials/material)
   :transform matrix/identity-matrix
   :inverse-transform matrix/identity-matrix
   :local-intersect intersect-sphere-space
   :normal compute-normal})
