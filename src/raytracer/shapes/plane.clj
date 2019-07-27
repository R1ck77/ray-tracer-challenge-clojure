(ns raytracer.shapes.plane
  (:require [raytracer.const :as const]
            [raytracer.tuple :as tuple]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]
            [raytracer.shapes.shared :as shared]
            [raytracer.material :as material]
            [raytracer.intersection :as intersection]
            [raytracer.shapes.bounding-box :as bounding-box]))

(defrecord Plane [material transform inverse-transform inverse-transposed-transform])

(defn- xz-plane-intersection [ray]
  (- (/ (:y (:origin ray))
        (:y (:direction ray)))))

(defn- intersect-plane-space [this-plane ray-in-plane-space]
  (if (< (Math/abs (float (:y (:direction ray-in-plane-space)))) const/EPSILON)
    []
    (vector (intersection/intersection (xz-plane-intersection ray-in-plane-space)
                                       this-plane))))

(extend-type Plane
  shared/Intersectable
  (local-intersect [this ray-in-plane-space]
    (intersect-plane-space this ray-in-plane-space))
  shared/Surface
  (compute-normal [this _]
    (shared/as-vector
     (matrix/transform (:inverse-transposed-transform this)
                       (svector/svector 0 1 0))))
    bounding-box/BoundingBox
    (get-corners [this]
      (vector (point/point const/neg-inf 0.0 const/neg-inf)
              (point/point const/inf 0.0 const/inf)))
    (hit [this ray] true)
    (get-transformed-points [this]
      (bounding-box/compute-filtered-transformed-extremes (bounding-box/get-corners this)
                                                          (:transform this))))

(defn plane []
  (map->Plane {:material (material/material)
               :transform matrix/identity-matrix
               :inverse-transform matrix/identity-matrix
               :inverse-transposed-transform matrix/identity-matrix}))
