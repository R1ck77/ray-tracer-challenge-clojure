(ns raytracer.shapes.group
  ;;; TODO/FIXME check require
  (:require [raytracer.const :as const]
            [raytracer.tuple :as tuple]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]
            [raytracer.ray :as ray]
            [raytracer.shapes.shared :as shared]
            [raytracer.material :as material]
            [raytracer.intersection :as intersection]
            [raytracer.shapes.bounding-box :as bounding-box]))

(def group)

(defn- intersect [group ray-object-space]
  (let [transformed-ray (ray/transform ray-object-space (:inverse-transform group))]
    (sort-by :t (reduce concat
                        (map #(shared/local-intersect % (ray/transform transformed-ray
                                                                       (:inverse-transform %)))
                             (:children group))))))

(defrecord Group [children inverse-transform inverse-transposed-transform]
  shared/Intersectable
  (local-intersect [this ray-object-space]
    (intersect this ray-object-space))
  shared/Surface
  (compute-normal [this point])
  bounding-box/BoundingBox
  (get-corners [this]
    (throw (UnsupportedOperationException. "TODO!"))))

(defn group [children]
  (->Group children
           matrix/identity-matrix
           matrix/identity-matrix))

(def empty-group (group []))

