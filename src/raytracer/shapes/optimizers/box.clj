(ns raytracer.shapes.optimizers.box
  (:require [raytracer.tuple :as tuple]
            [raytracer.point :as point]
            [raytracer.shapes.shared :as shared]
            [raytracer.shapes.bounding-box :as bounding-box]))

(defn- compare-points [component-predicate {xa :x, ya :y, za :z} {xb :x, yb :y, zb :z}]
  (and (component-predicate xa xb)
       (component-predicate ya yb)
       (component-predicate za zb)))

(defn- ordered? [a b]
  (compare-points <= a b))

(defn box [min-corner max-corner]
  {:pre [(ordered? min-corner max-corner)]}
  (bounding-box/create-box min-corner max-corner))

(defn contains-shape? [container shape]
  (let [merged-container (bounding-box/merge container
                                             (shared/get-bounding-box shape))]
    (and
     (tuple/all-c? (tuple/sub (:min-corner merged-container)
                              (:min-corner container))
                   #(>= % 0))
     (tuple/all-c? (tuple/sub (:max-corner container)
                              (:max-corner merged-container))
                   #(>= % 0)))))

(defn- create-sub-boxes
  "Returns a set of 8 vectors containing the points with the extremes of the original group bisection"
  [{min-x :x, min-y :y, min-z :z}
   {max-x :x, max-y :y, max-z :z}]
  (let [mid-x (/ (+ min-x max-x) 2)
        mid-y (/ (+ min-y max-y) 2)
        mid-z (/ (+ min-z min-z) 2)]
    (reduce #(if (contains? % %2)
               %
               (conj % %2)) #{}
            [(box (point/point min-x min-y min-z)
                  (point/point mid-x mid-y mid-z))
             (box (point/point mid-x min-y min-z)
                  (point/point max-x mid-y mid-z))
             (box (point/point min-x mid-y min-z)
                  (point/point mid-x max-y mid-z))
             (box (point/point mid-x mid-y min-z)
                  (point/point max-x max-y mid-z))
             (box (point/point min-x min-y mid-z)
                  (point/point mid-x mid-y max-z))
             (box (point/point mid-x min-y mid-z)
                  (point/point max-x mid-y max-z))
             (box (point/point min-x mid-y mid-z)
                  (point/point mid-x max-y max-z))
             (box (point/point mid-x mid-y mid-z)
                  (point/point max-x max-y max-z))])))

(defn bisect [box]
  (cond
    (bounding-box/infinite? box) [box]
    (bounding-box/invisible? box) [box]
    :default (create-sub-boxes (:min-corner box)
                               (:max-corner box))))
