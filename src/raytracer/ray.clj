(ns raytracer.ray
  (:require [raytracer.intersection :as intersection]
            [raytracer.shapes :as shapes]
            [raytracer.utils :as utils]
            [raytracer.tuple :as tuple]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]
            [raytracer.materials :as materials]))

(set! *unchecked-math* true)

(defrecord Ray [origin direction])

;;; TODO/FIXME use ray/ray instead (as for other objects)
(defn ray [origin direction]
  (->Ray origin direction))

(defn normalize [ray]
  (assoc ray
         :direction (svector/normalize (:direction ray))))

(defn position [ray t]
  (tuple/add (:origin ray)
             (svector/mul (:direction ray) t)))

(defn transform [input-ray matrix]
  (ray (matrix/transform matrix (:origin input-ray))
       (matrix/transform matrix (:direction input-ray))))

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

(defn intersect-sphere-space [ray-in-sphere-space sphere]
  (let [[a b c discriminant]
        (ray-sphere-discriminant ray-in-sphere-space sphere)]
    (if (< discriminant 0)
      []
      [(intersection/intersection (/ (- (- b) (Math/sqrt discriminant))
                                     (* 2 a))
                                  sphere)
       (intersection/intersection (/ (+ (- b) (Math/sqrt discriminant))
                                     (* 2 a))
                                  sphere)])))

(defn intersect [ray sphere]
  (intersect-sphere-space (transform ray (:inverse-transform sphere))
                          sphere))
