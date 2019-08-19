(ns raytracer.intersection
  (:require [raytracer.utils :as utils]
            [raytracer.tuple :as tuple]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]
            [raytracer.material :as material]))

(defprotocol TextureCoordinatesProvider
  (getUV [this] "Return a vector with the s and t coordinates or nil") )

(defrecord Intersection [t object]
  TextureCoordinatesProvider
  (getUV [this] nil))

(defn intersection [t object]
  (->Intersection t object))

(defrecord UVIntersection [t object u v]
  TextureCoordinatesProvider
  (getUV [this] [u v]))

(defn uv-intersection [t object u v]
  (->UVIntersection t object u v))

(defn intersections [ & args]
  (vec args))

(defn- non-backward? [intersection]
  (>= (:t intersection) 0))

(defn hit [xinters]
  (first (sort-by #(:t %) (filter non-backward? xinters))))
