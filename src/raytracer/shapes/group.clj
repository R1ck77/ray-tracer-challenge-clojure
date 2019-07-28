(ns raytracer.shapes.group
  ;;; TODO/FIXME check require
  (:require [raytracer.const :as const]
            [raytracer.point :as point]
            [raytracer.tuple :as tuple]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]
            [raytracer.ray :as ray]
            [raytracer.shapes.bounding-box :as bounding-box]            
            [raytracer.shapes.aabb-intersection :as aabb-intersection]
            [raytracer.shapes.shared :as shared]
            [raytracer.material :as material]
            [raytracer.intersection :as intersection]))

(def ^:dynamic *use-bounding-boxes* true)

(def group)

(defn- intersect [group ray-object-space]
  (let [transformed-ray (ray/transform ray-object-space (:inverse-transform group))]
    (sort-by :t (reduce concat
                        (map #(shared/local-intersect % (ray/transform transformed-ray
                                                                       (:inverse-transform %)))
                             (:children group))))))

(defn compute-extremes [children]
  (let [corners (mapcat bounding-box/get-transformed-points children)]
    (if (empty? corners)
      [(point/point 0 0 0) (point/point 0 0 0)]
      (bounding-box/extremes-from-points corners))))

(defprotocol Parent
  (get-children [this])
  (is-empty? [this])
  (set-children [this children]))

;;; TODO/FIXME the number of operations to do when you change the transform are
;;; starting to pile up
(defrecord Group [children transform inverse-transform inverse-transposed-transform aabb-extremes] ;;; TODO/FIXME this is really effed up
  shared/Intersectable
  (local-intersect [this ray-object-space]
    (intersect this ray-object-space))
  shared/Surface
  (compute-normal [this point]
    (throw (UnsupportedOperationException. "Normal of group computed")))
  bounding-box/BoundingBox
  (hit [this ray]
    (if *use-bounding-boxes*
      (aabb-intersection/hit aabb-extremes ray)
      true))
  (get-corners [this] ;;; TODO/FIXME this *has* to be cached at object creation
    (compute-extremes children))
  (get-transformed-points [this] ;;; TODO/FIXME and guess what? This too…
    (bounding-box/compute-filtered-transformed-extremes (bounding-box/get-corners this)
                                                        (:transform this)))
  Parent
  (get-children [this]
    children)
  (is-empty? [this] false)
  (set-children [this new-children]
    (merge this {:children new-children
                 :aabb-extremes (compute-extremes new-children)})))

(defrecord EmptyGroup [transform inverse-transform inverse-transposed-transform]
  shared/Intersectable
  (local-intersect [this ray-object-space]
    [])
  bounding-box/BoundingBox
  (hit [this ray] false)
  (get-corners [this]
    [(point/point 0 0 0) (point/point 0 0 0)])
  (get-transformed-points [this]
    [(point/point 0 0 0) (point/point 0 0 0)])
  Parent
  (get-children [this]
    [])
  (is-empty? [this] true)
  (set-children [this new-children]
    (->Group new-children
             (:transform this)
             (:inverse-transform this)
             (:inverse-transposed-transform this)
             (compute-extremes new-children))))

(defn group [children]
  (if (empty? children)
    (->EmptyGroup matrix/identity-matrix
                  matrix/identity-matrix
                  matrix/identity-matrix)
    (->Group children
             matrix/identity-matrix
             matrix/identity-matrix
             matrix/identity-matrix
             (compute-extremes children))))



(def empty-group (group []))

