(ns raytracer.shapes.bounding-box
  (:require [raytracer.point :as point]))

(defprotocol BoundingBox
  (get-sides [this] "Return each side of the box")
  (get-extremes [this] "Return the two extremes vertices of the box"))

(def unit-box (reify BoundingBox
                 (get-sides [this]
                   (point/point 2 2 2))
                 (get-extremes [this]
                   #{(point/point -1 -1 -1) (point/point 1 1 1)})))

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

(defn transform
  "Return an axis aligned bounding box transformed by transform"
  [this transform]
  ;;; Transform all the points and then get the new bounding box
  )

(defn merge
  "Return a bounding box that contains both boxes"
  [this other]
  ;;; Create the union of the points and get the new bounding box
  )
