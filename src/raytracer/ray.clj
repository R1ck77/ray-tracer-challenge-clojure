(ns raytracer.ray
  (:require [raytracer.tuple :as tuple]
            [raytracer.svector :as svector]))

(defrecord Ray [origin direction])

(defn create [origin direction]
  (->Ray origin direction))

(defn position [ray t]
  (tuple/add (:origin ray)
             (svector/mul (:direction ray) t)))

(defn sphere []
  nil)

(defn intersect [ray sphere]
  nil)
