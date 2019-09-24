(ns raytracer.shapes.bounding-box
  (:require [raytracer.const :as const]
            [raytracer.tuple :as tuple]
            [raytracer.point :as point]
            [raytracer.matrix :as matrix]
            [raytracer.shapes.aabb-intersection :as aabb-intersection]))

                                        ; TODO/FIXME use singletons for infinite and invisible boxes to spare memory
(def invisible-box)
(def infinite-box)

(defprotocol BoundingBox
  (transform [this matrix])
  (merge [this other] "Merge the two boxes and return a box that's the sum of both")
  (infinite? [this] "Return true if the box is infinite in any direction")
  (invisible? [this] "Return true if the box is 0 in all dimensions")
  (get-extremes [this] "Return the minimum and maximum corners of the box")
  (hit [this ray] "Returns true if the ray hits the box"))

(def create-box)

(defn- generic-merge [box-a box-b]
  (let [extremes-a (get-extremes box-a)
        extremes-b (get-extremes box-b)]
    (create-box  (tuple/op (:min extremes-a) (:min extremes-b) min)
                 (tuple/op (:max extremes-a) (:max extremes-b) max))))

(defn- merge-boxes [box-a box-b]
  (cond
    (infinite? box-b) box-b
    (invisible? box-b) box-a
    :default (generic-merge box-a box-b)))

(defrecord InvisibleBox []
  BoundingBox
  (transform [this matrix] this)
  (merge [this other] other)
  (infinite? [this] false)
  (invisible? [this] true)
  (get-extremes [this]
    (throw (UnsupportedOperationException. "Extremes of an invisible box requested")))
  (hit [this ray] false))

(def invisible-box (->InvisibleBox))

(defrecord InfiniteBox []
  BoundingBox
  (transform [this matrix] this) ; TODO/FIXME oversimplified
  (merge [this other] this)      ; TODO/FIXME oversimplified
  (infinite? [this] true)
  (invisible? [this] false)
  (get-extremes [this]
    (throw (UnsupportedOperationException. "Extremes of an infinite box requested")))
  (hit [this ray] true)) ;;; TODO/FIXME this is an oversimplification. We can do better

(def infinite-box (->InfiniteBox))

(def create-box)

(defn- filter-points [points f]
  {:pre [(not (empty? points))]}
  (reduce #(tuple/op % %2 f) points))

(defn extremes-from-points [points]
  {:pre [(not (empty? points))]}  
  (vector (filter-points points min)
          (filter-points points max)))

(defn- vertices-vectors-from-extremes [p1 p2]
  [[(:x p1) (:y p1) (:z p1)]
   [(:x p2) (:y p1) (:z p1)]
   [(:x p1) (:y p2) (:z p1)]
   [(:x p2) (:y p2) (:z p1)]
   [(:x p1) (:y p1) (:z p2)]
   [(:x p2) (:y p1) (:z p2)]
   [(:x p1) (:y p2) (:z p2)]
   [(:x p2) (:y p2) (:z p2)]])

(defn- box-points-from-extremes [p1 p2]
  (into #{} (map #(apply point/point %) (vertices-vectors-from-extremes p1 p2))))

(defn- transform-box-extremes [min-corner max-corner transform]
  (apply create-box (extremes-from-points
                     (map #(matrix/transform transform %)
                          (box-points-from-extremes min-corner max-corner)))))

(defrecord DefaultBox [min-corner max-corner]
  BoundingBox
  (transform [this matrix]
    (transform-box-extremes min-corner max-corner matrix))
  (merge [this other]
    (merge-boxes this other))
  (infinite? [this] false)
  (invisible? [this] false)
  (get-extremes [this] {:min min-corner
                        :max max-corner})
  (hit [this ray]
    (aabb-intersection/hit [min-corner max-corner] ray)))

(defn- is-infinite? [x]
  (>= (Math/abs ^double x) (/ const/inf 2)))

(defn- infinite-corners? [min-corner max-corner]
  (tuple/any-c? (tuple/sub max-corner min-corner) is-infinite?))

(defn- is-very-small? [x]
  (<= (Math/abs ^double x) const/EPSILON))

(defn- very-small-corners? [min-corner max-corner]
  (tuple/all-c? (tuple/sub max-corner min-corner) is-very-small?))

(defn create-box [min-corner max-corner]
  (cond 
    (infinite-corners? min-corner max-corner) infinite-box
    (very-small-corners? min-corner max-corner) invisible-box
    :default (->DefaultBox min-corner max-corner)))
