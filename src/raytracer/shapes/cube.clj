(ns raytracer.shapes.cube
  (:require [raytracer.tuple :as tuple]
            [raytracer.svector :as svector]
            [raytracer.color :as color]
            [raytracer.material :as material]
            [raytracer.shapes.shared :as shared]
            [raytracer.intersection :as intersection]))

(def EPSILON 1e-6) ;;; TODO/FIXME unify!

(defrecord Cube [material transform inverse-transform])

(defn- axis-intersection [numerator direction]
  (if (> (Math/abs direction) EPSILON)
    (/ numerator direction)
    (if (> numerator 0)
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

(extend-type Cube
  shared/Intersectable
  (local-intersect [this ray-object-space]
    (local-intersect this ray-object-space))
  shared/Surface
  (compute-normal [this point]
    nil))

(defn cube []
  (->Cube nil nil nil))

