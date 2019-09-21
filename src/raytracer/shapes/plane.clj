(ns raytracer.shapes.plane
  (:require [raytracer.const :as const]
            [raytracer.tuple :as tuple]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]
            [raytracer.shapes.shared :as shared]
            [raytracer.material :as material]
            [raytracer.intersection :as intersection]
            [raytracer.shapes.bounding-box :as bounding-box]
            [raytracer.shapes.placement :as placement]
            [raytracer.grouping.shared :as grshared]))

(defrecord Plane [material placement])

(def plane-bounding-box (bounding-box/->InfiniteBox))

(defn- xz-plane-intersection [ray]
  (- (/ (:y (:origin ray))
        (:y (:direction ray)))))

(defn- intersect-plane-space [this-plane ray-in-plane-space]
  (if (< (Math/abs (float (:y (:direction ray-in-plane-space)))) const/EPSILON)
    []
    (vector (intersection/intersection (xz-plane-intersection ray-in-plane-space)
                                       this-plane))))

(extend-type Plane
  shared/Transformable
  (change-transform [this transform-matrix]
    (placement/change-shape-transform this transform-matrix))
  (get-placement [this] (:placement this))
  shared/Intersectable
  (local-intersect [this ray-in-plane-space]
    (intersect-plane-space this ray-in-plane-space))
  (get-bounding-box [this]
    plane-bounding-box)
  shared/Surface
  (compute-normal
    ([this _ _ hierarchy]
     (shared/compute-normal this nil hierarchy))
    ([this _ hierarchy] ;;; TODO/FIXME does computing this every time make sense????
     (grshared/local-to-world-coordinates hierarchy this (svector/svector 0 1 0))))
  shared/Material
  (change-material [this new-material]
    (assoc this :material new-material))
  (get-material [this]
    (:material this))
  shared/Container
  (includes? [this object] (identical? this object)))

(defn plane []
  (map->Plane {:material (material/material)
               :placement (placement/placement)}))
