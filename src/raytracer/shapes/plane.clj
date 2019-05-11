(ns raytracer.shapes.plane
  (:require [raytracer.tuple :as tuple]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]
            [raytracer.shapes.shared :as shared]
            [raytracer.materials :as materials]
            [raytracer.intersection :as intersection]))

(set! *unchecked-math* true)

(def EPSILON 1e-6)

(defrecord Plane [material transform inverse-transform])

(defn- xz-plane-intersection [ray]
  (- (/ (:y (:origin ray))
        (:y (:direction ray)))))

(defn- intersect-plane-space [this-plane ray-in-plane-space]
  (if (< (Math/abs (float (:y (:direction ray-in-plane-space)))) EPSILON)
    []
    (vector (intersection/intersection (xz-plane-intersection ray-in-plane-space)
                                       this-plane))))

(extend-type Plane
  shared/Intersectable
  (local-intersect [this ray-in-plane-space]
    (intersect-plane-space this ray-in-plane-space))
  shared/Surface
  (compute-normal [_ _]
    (svector/svector 0 1 0)))

(defn plane []
  (map->Plane {:material (materials/material)
               :transform matrix/identity-matrix
               :inverse-transform matrix/identity-matrix}))
