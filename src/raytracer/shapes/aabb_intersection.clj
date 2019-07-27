(ns raytracer.shapes.aabb-intersection
  (:require [raytracer.const :as const]
            [raytracer.intersection :as intersection]))

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

(defn local-intersect [cube ray]
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
