(ns raytracer.shapes.cube
  (:require [raytracer.const :as const]
            [raytracer.tuple :as tuple]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]
            [raytracer.point :as point]
            [raytracer.color :as color]
            [raytracer.material :as material]
            [raytracer.shapes.shared :as shared]
            [raytracer.intersection :as intersection]))

(defrecord Cube [material transform inverse-transform])

(defn- axis-intersection [numerator direction]
  (if (> (Math/abs (float direction)) const/EPSILON)
    (/ numerator direction)
    (if (>= numerator 0)
      Double/POSITIVE_INFINITY
      Double/NEGATIVE_INFINITY)))

(defn- check-axis [origin direction]
  (let [a (axis-intersection (- 0 1 origin) direction)
        b (axis-intersection (- 1 origin) direction)]
    (if (> a b) [b a] [a b])))

(defn- local-intersect [cube ray]
  (let [[tminx tmaxx] (check-axis (:x (:origin ray)) (:x (:direction ray)))
        [tminy tmaxy] (check-axis (:y (:origin ray)) (:y (:direction ray)))
        [tminz tmaxz] (check-axis (:z (:origin ray)) (:z (:direction ray)))]
    (let [t1 (max tminx tminy tminz)
          t2 (min tmaxx tmaxy tmaxz)]
      (if (> t2 t1) 
        [(intersection/intersection t1 cube)
         (intersection/intersection t2 cube)]
        []))))

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
    ;;; TODO/FIXME test needed
    (tuple/normalize
     (shared/as-vector
      (matrix/transform (matrix/transpose (:inverse-transform this))
                        (compute-cube-normal this
                                             ;;; TODO/FIXME test needed…
                                             (matrix/transform (:inverse-transform this) point)))))))

;;; TODO/FIXME test needed
(defn cube []
  (->Cube (material/material)
          matrix/identity-matrix
          matrix/identity-matrix))

