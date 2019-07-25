(ns raytracer.shapes.cube
  (:require [raytracer.const :as const]
            [raytracer.tuple :as tuple]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]
            [raytracer.point :as point]
            [raytracer.material :as material]
            [raytracer.shapes.shared :as shared]
            [raytracer.intersection :as intersection]
            [raytracer.shapes.bounding-box :as bounding-box]))

(defrecord Cube [material inverse-transform inverse-transposed-transform])

(defn- signed-infinity [numerator _]
  (if (>= numerator 0)
    Double/POSITIVE_INFINITY
    Double/NEGATIVE_INFINITY))

(defn- check-axis [origin direction]
  (let [operator (if (> (Math/abs (float direction)) const/EPSILON)
                   /
                   signed-infinity)]
    (let [a (operator (- 0 1 origin) direction)
          b (operator (- 1 origin) direction)]
      (if (> a b) [b a] [a b]))))

(defn- local-intersect [cube ray]
  (let [origin (:origin ray)
        direction (:direction ray)]
    (let [[tminx tmaxx] (check-axis (:x origin) (:x direction))
          [tminy tmaxy] (check-axis (:y origin) (:y direction))
          min (Math/max (float  tminx) (float tminy))
          max (Math/min (float tmaxx) (float tmaxy))]
      (if (> min max)
        []
        (let [[tminz tmaxz] (check-axis (:z origin) (:z direction))
              min (Math/max (float min) (float tminz))
              max (Math/min (float max) (float tmaxz))]
          (if (> min max)
            []
            [(intersection/intersection min cube)
             (intersection/intersection max cube)]))))))

(defn- sign [value]
  (if (< value 0) -1 1))

(defn- compare [point field-1 field-2]
  (if (> (Math/abs (float (get point field-1)))
         (Math/abs (float (get point field-2))))
    field-1
    field-2))

(defn- get-field-of-largest-component [point]
  (compare point :z (compare point :y :x)))

(defn- compute-cube-normal [cube point]
  (let [field (get-field-of-largest-component point)
        value (sign (get point field))]
    (cond
      (= field :x) (svector/svector value 0 0)
      (= field :y) (svector/svector 0 value 0)
      (= field :z) (svector/svector 0 0 value)
      :default (throw (IllegalStateException. "Unexpected condition in cube/compute-normal")))))

(extend-type Cube
  shared/Intersectable
  (local-intersect [this ray-object-space]
    (local-intersect this ray-object-space))
  shared/Surface
  (compute-normal [this point]
    (tuple/normalize
     (shared/as-vector
      (matrix/transform (:inverse-transposed-transform this)
                        (compute-cube-normal this
                                             (matrix/transform (:inverse-transform this) point))))))
  bounding-box/BoundingBox
  (get-corners [this]
    (vector (point/point -1 -1 -1)
            (point/point 1 1 1))))

(defn cube []
  (->Cube (material/material)
          matrix/identity-matrix
          matrix/identity-matrix))

