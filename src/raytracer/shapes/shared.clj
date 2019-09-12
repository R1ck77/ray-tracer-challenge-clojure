(ns raytracer.shapes.shared
  (:require [raytracer.point :as point]
            [raytracer.matrix :as matrix]
            [raytracer.svector :as svector]))

(defprotocol Intersectable
  (local-intersect [this ray]))

(defprotocol Transformable
  (change-transform [this new-transform])
  (get-placement [this]))

(defprotocol Surface
  (compute-normal [this point] [this point intersection]))

(defprotocol Material
  (change-material [this new-material] "Return a new object with the material changed")
  (get-material [this] "Return the shape's current material"))

(defprotocol Container
  (includes? [this object]))

(defn as-point [v]
  (point/point (:x v) (:y v) (:z v)))

(defn as-vector [p]
  (svector/svector (:x p) (:y p) (:z p)))
