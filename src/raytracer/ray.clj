(ns raytracer.ray
  (:require [raytracer.intersection :as intersection]
            [raytracer.shapes :as shapes]
            [raytracer.shapes.shared :as shared]
            [raytracer.utils :as utils]
            [raytracer.tuple :as tuple]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]
            [raytracer.material :as material]))

(defprotocol RayCaster
  (intersect [this direction]))

(defrecord Ray [origin direction])

(defn ray [origin shape]
  (->Ray origin shape))

;;; TODO/FIXME this accounts for 20% of execution time as of 25/5/19
(defn transform [input-ray matrix]
  (ray (matrix/transform matrix (:origin input-ray))
       (matrix/transform matrix (:direction input-ray))))

(extend-type Ray
  RayCaster
  (intersect [this shape]
    (shared/local-intersect shape (transform this (:inverse-transform shape)))))


(defn normalize [ray]
  (assoc ray
         :direction (tuple/normalize (:direction ray))))

(defn position [ray t]
  (tuple/add (:origin ray)
             (tuple/mul (:direction ray) t)))





