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

(defn intersect [ray shape]
  ((:local-intersect shape) shape (transform ray (:inverse-transform shape))))
