(ns raytracer.shapes.optimizers.box
  (:require [raytracer.point :as point]
            [raytracer.shapes.bounding-box :as bounding-box]))

(defrecord Box [min-point max-point])

(defn- compare-points [component-predicate {xa :x, ya :y, za :z} {xb :x, yb :y, zb :z}]
  (and (component-predicate xa xb)
       (component-predicate ya yb)
       (component-predicate za zb)))

(defn- ordered? [a b]
  (compare-points <= a b))

(defn box [min-point max-point]
  {:pre [(ordered? min-point max-point)]}
  (->Box min-point max-point))

(defn- contains-shape [box shape]
  (let [shape-box (apply box (bounding-box/get-transformed-points shape))]
    (and (compare-points <= (:min-point box) (:min-point shape-box))
         (compare-points >= (:max-point box) (:min-point shape-box)))))

(defn- create-sub-boxes
  "Returns a set of 8 vectors containing the points with the extremes of the original group bisection"
  [{min-x :x, min-y :y, min-z :z}
   {max-x :x, max-y :y, max-z :z}]
  (let [mid-x (/ (+ min-x max-x) 2)
        mid-y (/ (+ min-y max-y) 2)
        mid-z (/ (+ min-z min-z) 2)]
    #{(->Box (point/point min-x min-y min-z)
             (point/point mid-x mid-y mid-z))
      (->Box (point/point mid-x min-y min-z)
             (point/point max-x mid-y mid-z))
      (->Box (point/point min-x mid-y min-z)
             (point/point mid-x max-y mid-z))
      (->Box (point/point mid-x mid-y min-z)
             (point/point max-x max-y mid-z))
      (->Box (point/point min-x min-y mid-z)
             (point/point mid-x mid-y max-z))
      (->Box (point/point mid-x min-y mid-z)
             (point/point max-x mid-y max-z))
      (->Box (point/point min-x mid-y mid-z)
             (point/point mid-x max-y max-z))
      (->Box (point/point mid-x mid-y mid-z)
             (point/point max-x max-y max-z))}))

(defprotocol  Partitionable
  (bisect [this] "Returns a set of 8 boxes that bisect this one in 3D"))

(defprotocol Container
  (contains [this shape] "Returns true if the shape is inside the box"))

(extend-type Box
  Partitionable
  (bisect [this]
    (create-sub-boxes (:min-point this)
                      (:max-point this)))
  Container
  (contains [this shape]
    (contains-shape this shape)))
