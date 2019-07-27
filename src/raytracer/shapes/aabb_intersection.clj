(ns raytracer.shapes.aabb-intersection
  (:require [raytracer.const :as const]
            [raytracer.point :as point]
            [raytracer.intersection :as intersection]))

(def unit-aabb-min-point (point/point -1 -1 -1))
(def unit-aabb-max-point (point/point 1 1 1))
(def unit-aabb-extremes [unit-aabb-min-point unit-aabb-max-point])

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

(defn local-intersect-t [[min-point max-point] ray]
  (let [origin (:origin ray)
        direction (:direction ray)]
    (let [[tminx tmaxx] (check-axis (:x min-point) (:x max-point) (:x origin) (:x direction))
          [tminy tmaxy] (check-axis (:y min-point) (:y max-point) (:y origin) (:y direction))
          min (Math/max (float tminx) (float tminy))
          max (Math/min (float tmaxx) (float tmaxy))]
      (if (> min max)
        []
        (let [[tminz tmaxz] (check-axis (:z min-point) (:z max-point) (:z origin) (:z direction))
              min (Math/max (float min) (float tminz))
              max (Math/min (float max) (float tmaxz))]
          (if (> min max)
            []
            [min max]))))))

(defn hit [extremes ray]
  (not (empty? (local-intersect-t extremes ray))))

(defn local-intersect [cube ray]
  ;;; TODO/FIXME potentially slow. May be worth an "if"
  (mapv #(intersection/intersection % cube)
        (local-intersect-t unit-aabb-extremes ray)))
