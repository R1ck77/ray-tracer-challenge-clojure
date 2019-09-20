(ns raytracer.shapes.bounding-box
  (:require [raytracer.const :as const]
            [raytracer.tuple :as tuple]
            [raytracer.point :as point]
            [raytracer.matrix :as matrix]
            [raytracer.shapes.aabb-intersection :as aabb-intersection]))

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

(defrecord InfiniteBox []
  BoundingBox
  (transform [this matrix] this) ; TODO/FIXME oversimplified
  (merge [this other] this)      ; TODO/FIXME oversimplified
  (infinite? [this] true)
  (invisible? [this] false)
  (get-extremes [this]
    {:min (point/point const/neg-inf const/neg-inf const/neg-inf)
     :max (point/point const/inf const/inf const/inf)})
  (hit [this ray] true)) ;;; TODO/FIXME this is an oversimplification. We can do better

(defrecord DefaultBox [min-corner max-corner]
  BoundingBox
  (transform [this matrix])
  (merge [this other]
    (merge-boxes this other))
  (infinite? [this] false)
  (invisible? [this] false)
  (get-extremes [this] min-corner max-corner)
  (hit [this ray]
    (aabb-intersection/hit [min-corner max-corner] ray)))

(defn- is-infinite? [x]
  (>= (Math/abs ^double x) const/inf))

(defn- infinite-corners [min-corner max-corner]
  (or (tuple/any-c? min-corner is-infinite?)
      (tuple/any-c? max-corner is-infinite?)))

(defn- is-very-small? [x]
  (<= (Math/abs ^double x) const/EPSILON))

(defn- very-small-corners [min-corner max-corner]
  (or (tuple/any-c? min-corner is-very-small?)
        (tuple/any-c? max-corner is-very-small?)))

(defn create-box [min-corner max-corner]
  (cond 
    (infinite-corners min-corner max-corner) (->InfiniteBox)
    (very-small-corners min-corner max-corner) (->InvisibleBox)
    :default (->DefaultBox min-corner max-corner)))


(defprotocol BoundingBox
  (get-corners [this] "Return the two extremes vertices of the box")
  (get-transformed-extremes [this] "Return 8 points in parent space, or an empty collection if the object is point-like")
  (hit [this ray] "Returns true if the ray hit the bounding box"))

(def unit-box (reify BoundingBox
                 (get-corners [this]
                   [(point/point -1 -1 -1) (point/point 1 1 1)])))

(defn- to-vector [point]
  (vector (:x point)
          (:y point)
          (:z point)))

(defn combine-points [f point1 point2]
  (apply point/point
         (map f (to-vector point1) (to-vector point2))))

(defn- filter-points [points f]
  {:pre [(not (empty? points))]}
  (reduce (partial combine-points f)
          points))

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

(defn box-points-from-extremes [p1 p2]
  (into #{}  (map #(apply point/point %) (vertices-vectors-from-extremes p1 p2))))

(defn almost-identical [point1 point2]
  (and
   (< (Math/abs (float (- (:x point1) (:x point2)))) const/EPSILON)
   (< (Math/abs (float (- (:y point1) (:y point2)))) const/EPSILON)
   (< (Math/abs (float (- (:z point1) (:z point2)))) const/EPSILON)
   (< (Math/abs (float (- (:w point1) (:w point2)))) const/EPSILON)))

(defn transform-extremes [extremes transform]
  (extremes-from-points
   (map #(matrix/transform transform %)
        (apply box-points-from-extremes extremes))))

(defn compute-filtered-transformed-extremes [extremes transform]
  (let [result (transform-extremes extremes transform)]
    (if (apply almost-identical result)
      []
      result)))
