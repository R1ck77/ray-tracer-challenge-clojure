(ns raytracer.shapes.bounding-box
  (:require [raytracer.const :as const]
            [raytracer.point :as point]
            [raytracer.matrix :as matrix]))

(defprotocol BoundingBox
  (get-corners [this] "Return the two extremes vertices of the box")
  ;;; TODO/FIXME change this into "transformed-extremes"
  (get-transformed-points [this] "Return 8 points in parent space, or an empty collection if the object is point-like")
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
  #{[(:x p1) (:y p1) (:z p1)]
    [(:x p2) (:y p1) (:z p1)]
    [(:x p1) (:y p2) (:z p1)]
    [(:x p2) (:y p2) (:z p1)]
    [(:x p1) (:y p1) (:z p2)]
    [(:x p2) (:y p1) (:z p2)]
    [(:x p1) (:y p2) (:z p2)]
    [(:x p2) (:y p2) (:z p2)]})

(defn box-points-from-extremes [p1 p2]
  (into #{}  (map #(apply point/point %) (vertices-vectors-from-extremes p1 p2))))

(defn almost-identical [point1 point2]
  (and
   (< (Math/abs (float (- (:x point1) (:x point2)))) const/EPSILON)
   (< (Math/abs (float (- (:y point1) (:y point2)))) const/EPSILON)
   (< (Math/abs (float (- (:z point1) (:z point2)))) const/EPSILON)
   (< (Math/abs (float (- (:w point1) (:w point2)))) const/EPSILON)))

(defn- transform-extremes [extremes transform]
  (extremes-from-points
   (map #(matrix/transform transform %)
        (apply box-points-from-extremes extremes))))

(defn compute-filtered-transformed-extremes [extremes transform]
  (let [result (transform-extremes extremes transform)]
    (if (apply almost-identical result)
      []
      result)))
