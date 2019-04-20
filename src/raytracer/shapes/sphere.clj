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
        a (svector/dot (:direction ray)
                       (:direction ray))
        b (* 2 (svector/dot (:direction ray)
                            sphere-to-ray))
        c (- (svector/dot sphere-to-ray sphere-to-ray) 1)]
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

(defn create-compute-normal-f [shape]
  (fn [point]
    (svector/normalize
     (shared/as-vector
      (matrix/transform (matrix/transpose (:inverse-transform shape))
                        (tuple/sub (matrix/transform (:inverse-transform shape) point)
                                   (point/point 0 0 0)))))))

(defn add-normal-f [shape]
  (assoc shape :normal (create-compute-normal-f shape)))

(defn sphere []
  (add-normal-f {:center [0 0 0 1]
                 :radius 1.0
                 :material (materials/material)
                 :transform matrix/identity-matrix
                 :inverse-transform matrix/identity-matrix
                 :local-intersect intersect-sphere-space}))
