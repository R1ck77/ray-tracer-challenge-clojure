(ns raytracer.shapes.cube
  (:require [raytracer.tuple :as tuple]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]
            [raytracer.point :as point]
            [raytracer.material :as material]
            [raytracer.shapes.shared :as shared]
            [raytracer.shapes.aabb-intersection :as aabb-intersection]
            [raytracer.shapes.bounding-box :as bounding-box]
            [raytracer.shapes.placement :as placement]))

(defrecord Cube [material placement])

(def cube-bounding-box (bounding-box/create-box (point/point -1 -1 -1)
                                                (point/point 1 1 1)))

(defn- sign [value]
  (if (< value 0) -1 1))

(defn- compare-coordinate [point field-1 field-2]
  (if (> (Math/abs (float (get point field-1)))
         (Math/abs (float (get point field-2))))
    field-1
    field-2))

(defn- get-field-of-largest-component [point]
  (compare-coordinate point :z (compare-coordinate point :y :x)))

(defn- compute-cube-normal [cube point]
  (let [field (get-field-of-largest-component point)
        value (sign (get point field))]
    (cond
      (= field :x) (svector/svector value 0 0)
      (= field :y) (svector/svector 0 value 0)
      (= field :z) (svector/svector 0 0 value)
      :default (throw (IllegalStateException. "Unexpected condition in cube/compute-normal")))))

(extend-type Cube
  shared/Transformable
  (change-transform [this transform-matrix]
    (placement/change-shape-transform this transform-matrix))
  (get-placement [this] (:placement this))
  shared/Intersectable
  (local-intersect [this ray-object-space]
    (aabb-intersection/local-intersect this ray-object-space))
  (get-bounding-box [this]
    (bounding-box/transform cube-bounding-box
                            (placement/get-transform (:placement this))))
  shared/Surface
  (compute-normal
    ([this point _ hierarchy]
     (shared/compute-normal this point hierarchy))
    ([this point hierarchy]
     (shared/decorated-compute-normal compute-cube-normal
                                      this
                                      point
                                      hierarchy)))
  shared/Material
  (change-material [this new-material]
    (assoc this :material new-material))
  (get-material [this]
    (:material this))
  shared/Container
  (includes? [this object] (identical? this object)))

(defn cube []
  (->Cube (material/material)
          (placement/placement)))
