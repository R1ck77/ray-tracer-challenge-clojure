(ns raytracer.shapes.aabb-intersection
  (:require [raytracer.const :as const]
            [raytracer.intersection :as intersection]))

(defn- signed-infinity [numerator _]
  (if (>= numerator 0)
    Double/POSITIVE_INFINITY
    Double/NEGATIVE_INFINITY))

(defn- check-axis
  ([origin direction]
   (check-axis -1 1 origin direction))
  ([axis-min axis-max origin direction]
   (let [operator (if (> (Math/abs (float direction)) const/EPSILON)
                    /
                    signed-infinity)]
     (let [a (operator (- axis-min origin) direction)
           b (operator (- axis-max origin) direction)]
       (if (> a b) [b a] [a b])))))

(defn local-intersect-t [ray]
  (let [origin (:origin ray)
        direction (:direction ray)]
    (let [[tminx tmaxx] (check-axis -1 1 (:x origin) (:x direction))
          [tminy tmaxy] (check-axis -1 1(:y origin) (:y direction))
          min (Math/max (float  tminx) (float tminy))
          max (Math/min (float tmaxx) (float tmaxy))]
      (if (> min max)
        []
        (let [[tminz tmaxz] (check-axis (:z origin) (:z direction))
              min (Math/max (float min) (float tminz))
              max (Math/min (float max) (float tmaxz))]
          (if (> min max)
            []
            [min max]))))))

(defn local-intersect [cube ray]
  ;;; TODO/FIXME potentially slow. May be worth an "if"
  (mapv #(intersection/intersection % cube)
        (local-intersect-t ray)))
