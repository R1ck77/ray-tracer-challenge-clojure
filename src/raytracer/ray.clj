(ns raytracer.ray
  (:require [raytracer.intersection :as intersection]
            [raytracer.shapes.shared :as shared]
            [raytracer.utils :as utils]
            [raytracer.tuple :as tuple]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]
            [raytracer.material :as material]
            [raytracer.shapes.bounding-box :as bounding-box]))

(defprotocol RayCaster
  (intersect [this direction]))

(defrecord Ray [origin direction])

(defn ray [origin shape]
  (->Ray origin shape))

(defn transform [input-ray matrix]
  (ray (matrix/transform matrix (:origin input-ray))
       (matrix/transform matrix (:direction input-ray))))

(extend-type Ray
  RayCaster
  (intersect [this shape]
    (let [object-space-ray (transform this (:inverse-transform shape))]
     (if (bounding-box/hit shape this)
       (shared/local-intersect shape object-space-ray)
       []))))

(defn normalize [ray]
  (assoc ray
         :direction (tuple/normalize (:direction ray))))

(defn position [ray t]
  (tuple/add (:origin ray)
             (tuple/mul (:direction ray) t)))
