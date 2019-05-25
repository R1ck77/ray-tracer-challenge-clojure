(ns raytracer.shapes.cube
  (:require [raytracer.const :as const]
            [raytracer.tuple :as tuple]
            [raytracer.svector :as svector]
            [raytracer.point :as point]
            [raytracer.color :as color]
            [raytracer.material :as material]
            [raytracer.shapes.shared :as shared]
            [raytracer.intersection :as intersection])
  (:import [raytracer CheckAxis]))

(defrecord Cube [material transform inverse-transform])

(defn- local-intersect [cube ray]
  (let [origin (:origin ray)
        direction (:direction ray)
        t-list (CheckAxis/localIntersect (:x origin) (:y origin) (:z origin)
                                         (:x direction) (:y direction) (:z direction))]
    (if t-list
      [(intersection/intersection (aget t-list 0) cube)
       (intersection/intersection (aget t-list 1) cube)]
      []))
)

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
    (compute-cube-normal this point)))

(defn cube []
  (->Cube nil nil nil))

