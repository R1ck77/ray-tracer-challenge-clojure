(ns raytracer.shapes.shared
  (:require [raytracer.point :as point]
            [raytracer.matrix :as matrix]
            [raytracer.svector :as svector]))

(defprotocol Intersectable
  (local-intersect [this ray]))

(defprotocol Transformable
  (change-transform [this new-transform]))

(defprotocol Surface
  (compute-normal [this point] [this point intersection]))

(defn as-point [v]
  (point/point (:x v) (:y v) (:z v)))

(defn as-vector [p]
  (svector/svector (:x p) (:y p) (:z p)))
