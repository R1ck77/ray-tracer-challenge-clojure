(ns raytracer.shapes.shared
  (:require [raytracer.point :as point]
            [raytracer.matrix :as matrix]
            [raytracer.svector :as svector]
            [raytracer.grouping.shared :as gshared]))

(defprotocol Intersectable
  (local-intersect [this ray]))

(defprotocol Transformable
  (change-transform [this new-transform])
  (get-placement [this]))

(defprotocol Surface
  (compute-normal [this point hierarchy] [this point intersection hierarchy]))

(defprotocol Material
  (change-material [this new-material] "Return a new object with the material changed")
  (get-material [this] "Return the shape's current material"))

(defprotocol Container
  (includes? [this object]))

(defn as-point [v]
  (point/point (:x v) (:y v) (:z v)))

(defn as-vector [p]
  (svector/svector (:x p) (:y p) (:z p)))

;;; TODO/FIXME this shold go in a class (not clear which one)
(defn decorated-compute-normal [local-compute-normal shape point hierarchy]
  (let [local-normal-vector (local-compute-normal shape
                                                  (if point (gshared/world-to-local-coordinates hierarchy shape point) nil))]
    (gshared/local-to-world-coordinates hierarchy shape local-normal-vector)))
