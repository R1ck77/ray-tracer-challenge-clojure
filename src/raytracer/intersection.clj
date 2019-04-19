(ns raytracer.intersection
  (:require [raytracer.utils :as utils]
            [raytracer.tuple :as tuple]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]
            [raytracer.materials :as materials]))

(set! *unchecked-math* true)

(defrecord Intersection [t object])

(defn intersection [t object]
  (->Intersection t object))

(defn intersections [ & args]
  (vec args))

(defn- non-backward? [intersection]
  (>= (:t intersection) 0))

(defn hit [xinters]
  (first (sort-by #(:t %) (filter non-backward? xinters))))
