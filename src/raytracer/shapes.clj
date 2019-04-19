(ns raytracer.shapes
  (:require [raytracer.utils :as utils]
            [raytracer.tuple :as tuple]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]
            [raytracer.materials :as materials]))

(set! *unchecked-math* true)

(defn as-point [[x y z _]]
  (point/point x y z))

(defn as-vector [[x y z _]]
  (svector/svector x y z))

(defn create-compute-normal-f [shape]
  (fn [point]
    (svector/normalize
     (as-vector
      (matrix/transform (matrix/transpose (:inverse-transform shape))
                        (tuple/sub (matrix/transform (:inverse-transform shape) point)
                                   (point/point 0 0 0)))))))

(defn- add-normal-f [shape]
  (assoc shape :normal (create-compute-normal-f shape)))

(defn sphere []
  (add-normal-f {:center [0 0 0 1]
                 :radius 1.0
                 :material (materials/material)
                 :transform matrix/identity-matrix
                 :inverse-transform matrix/identity-matrix}))

(defn same-shape? [a b]
  (if (= (merge a {:normal nil})
         (merge b {:normal nil}))
    a
    nil))

(defn change-transform [shape new-transform]
  (add-normal-f (merge shape {:transform new-transform
                               :inverse-transform (matrix/invert new-transform 4)})))

(defn change-material [shape new-material]
  (assoc shape :material new-material))

;;; TODO/FIXME count/values is completely useless!!!
(defn ray-sphere-discriminant [ray sphere]
  (let [sphere-to-ray (tuple/sub (:origin ray)
                                 (:center sphere))
        a (svector/dot (:direction ray)
                       (:direction ray))
        b (* 2 (svector/dot (:direction ray)
                            sphere-to-ray))
        c (- (svector/dot sphere-to-ray sphere-to-ray) 1)]
    [a b c (- (* b b ) (* 4 a c))]))
