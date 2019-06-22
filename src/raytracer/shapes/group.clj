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
  (sort-by :t (reduce concat
                      (map #(shared/local-intersect % ray-object-space)
                           (:children group)))))

(defrecord Group [children inverse-transform inverse-transpose-transform]
  shared/Intersectable
  (local-intersect [this ray-object-space]
    (intersect this ray-object-space))
  shared/Surface
  (compute-normal [this point]))

(defn group [children]
  (->Group children
           matrix/identity-matrix
           matrix/identity-matrix))

(def empty-group (group []))

