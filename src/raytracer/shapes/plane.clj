(ns raytracer.shapes.plane
  (:require [raytracer.tuple :as tuple]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]
            [raytracer.shapes.shared :as shared]
            [raytracer.materials :as materials]
            [raytracer.intersection :as intersection]))

(set! *unchecked-math* true)

(defn- intersect-plane-space [this-plane ray-in-plane-space]
)

(defn plane []
  {:material (materials/material)
   :transform matrix/identity-matrix
   :inverse-transform matrix/identity-matrix
   :local-intersect intersect-plane-space
   :normal (fn [_] (svector/svector 0 1 0))})
