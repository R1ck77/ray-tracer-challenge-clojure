(ns raytracer.shapes.plane
  (:require [raytracer.tuple :as tuple]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]
            [raytracer.shapes.shared :as shared]
            [raytracer.materials :as materials]
            [raytracer.intersection :as intersection]))

(set! *unchecked-math* true)

(def EPSILON 1e-6)

(defn- xz-plane-intersection [ray]
  (- (/ (second (:origin ray))
        (second (:direction ray)))))

(defn- intersect-plane-space [this-plane ray-in-plane-space]
  (if (< (Math/abs (second (:direction ray-in-plane-space))) EPSILON)
    []
    (vector {:t (xz-plane-intersection ray-in-plane-space)
             :object this-plane})))

(defn plane []
  {:material (materials/material)
   :transform matrix/identity-matrix
   :inverse-transform matrix/identity-matrix
   :local-intersect intersect-plane-space
   :normal (fn [_ _] (svector/svector 0 1 0))})
