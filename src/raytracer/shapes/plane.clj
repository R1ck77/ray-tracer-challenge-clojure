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
            [raytracer.shapes.placement :as placement]))

(defrecord Plane [material placement])

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
  shared/Intersectable
  (local-intersect [this ray-in-plane-space]
    (intersect-plane-space this ray-in-plane-space))
  shared/Surface
  (compute-normal
    ([this _ _]
     (shared/compute-normal this nil))
    ([this _]
     (shared/as-vector
      (matrix/transform (-> this :placement placement/get-inverse-transposed-transform)
                        (svector/svector 0 1 0)))))
    bounding-box/BoundingBox
    (get-corners [this]
      (vector (point/point const/neg-inf 0.0 const/neg-inf)
              (point/point const/inf 0.0 const/inf)))
    (hit [this ray] true)
    (get-transformed-extremes [this]
      (bounding-box/compute-filtered-transformed-extremes (bounding-box/get-corners this)
                                                          (-> this :placement placement/get-transform)))
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
