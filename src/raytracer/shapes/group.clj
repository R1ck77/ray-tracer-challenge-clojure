(ns raytracer.shapes.group
  ;;; TODO/FIXME check require
  (:require [raytracer.const :as const]
            [raytracer.point :as point]
            [raytracer.tuple :as tuple]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]
            [raytracer.ray :as ray]
            [raytracer.shapes.bounding-box :as bounding-box]            
            [raytracer.shapes.shared :as shared]
            [raytracer.material :as material]
            [raytracer.intersection :as intersection]))

(def group)

(defn- intersect [group ray-object-space]
  (let [transformed-ray (ray/transform ray-object-space (:inverse-transform group))]
    (sort-by :t (reduce concat
                        (map #(shared/local-intersect % (ray/transform transformed-ray
                                                                       (:inverse-transform %)))
                             (:children group))))))

;;; TODO/FIXME the number of operations to do when you change the transform are
;;; starting to pile up
(defrecord Group [children inverse-transform inverse-transposed-transform]
  shared/Intersectable
  (local-intersect [this ray-object-space]
    (intersect this ray-object-space))
  shared/Surface
  (compute-normal [this point])
  bounding-box/BoundingBox
  (hit [this ray]
    (throw (UnsupportedOperationException. "not supported (yet)")))
  (get-corners [this]  ;;; TODO/FIXME this *has* to be cached at object creation
    #_(if (empty? children)
      (vector (point/point 0.0 0.0 0.0) ;;; I don't like that
              (point/point 0.0 0.0 0.0))
      (bounding-box/extremes-from-points
       (mapcat get-transformed-points children))))
  (get-transformed-points [this]
   #_ (bounding-box/compute-transformed-points (get-corners this)
                                             (:transform this))))

(defn group [children]
  (->Group children
           matrix/identity-matrix
           matrix/identity-matrix))

(def empty-group (group []))

