(ns raytracer.shapes.cube
  (:require [raytracer.const :as const]
            [raytracer.tuple :as tuple]
            [raytracer.svector :as svector]
            [raytracer.point :as point]
            [raytracer.color :as color]
            [raytracer.material :as material]
            [raytracer.shapes.shared :as shared]
            [raytracer.intersection :as intersection]))

(defrecord Cube [material transform inverse-transform])

(defn- axis-intersection ^double [numerator direction]
  (if (> (Math/abs (float direction)) const/EPSILON)
    (/ numerator direction)
    (if (> numerator 0)
      Double/POSITIVE_INFINITY
      Double/NEGATIVE_INFINITY)))

(defprotocol TmpContent
  (getOrigin [this])
  (setValues [this new-origin new-direction])
  (getDirection [this]))

(deftype Tmp [^:unsynchronized-mutable origin
              ^:unsynchronized-mutable direction]
  TmpContent
  (getOrigin [this]
    (.origin this))
  (setValues [this new-origin new-direction]
    (set! origin new-origin)
    (set! direction new-direction))
  (getDirection [this]
    (.direction this)))

(defn- check-axis [^Tmp tmp ^doubles min-array ^doubles max-array ^long index]
  (let [a (axis-intersection (- 0 1 (.getOrigin tmp)) (.getDirection tmp))
        b (axis-intersection (- 1 (.getOrigin tmp)) (.getDirection tmp))]
    (if (> a b)
      (do
        (aset min-array index b)
        (aset max-array index a))
      (do
        (aset min-array index a)
        (aset max-array index b)))))

(defn- local-intersect [cube ray]
  (let [direction (:direction ray)
        origin (:origin ray)
        min-array (double-array 3)
        max-array (double-array 3)]
    (let [tmp ^Tmp (->Tmp (:x origin) (:x direction))]
      (check-axis tmp min-array max-array 0)
      (.setValues tmp (:y origin) (:y direction))
      (check-axis tmp min-array max-array 1)
      (.setValues tmp (:z origin) (:z direction))
      (check-axis tmp min-array max-array 2)
      (let [t1 (Math/max (aget min-array 0) (Math/max (aget min-array 1) (aget min-array 2)))
            t2 (Math/min (aget max-array 0) (Math/min (aget max-array 1) (aget max-array 2)))]
        (if (> t2 t1) 
          [(intersection/intersection t1 cube)
           (intersection/intersection t2 cube)]
          [])))))

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

