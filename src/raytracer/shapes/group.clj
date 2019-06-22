(ns raytracer.shapes.group
  ;;; TODO/FIXME check require
  (:require [raytracer.const :as const]
            [raytracer.tuple :as tuple]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]
            [raytracer.shapes.shared :as shared]
            [raytracer.material :as material]
            [raytracer.intersection :as intersection]))

(def group)

(defn- intersect [group ray-object-space]
  [])

(defrecord Group [inverse-transform inverse-transpose-transform]
  shared/Intersectable
  (local-intersect [this ray-object-space]
    (intersect group ray-object-space))
  shared/Surface
  (compute-normal [this point]))

(defn group []
  (->Group matrix/identity-matrix
           matrix/identity-matrix))



