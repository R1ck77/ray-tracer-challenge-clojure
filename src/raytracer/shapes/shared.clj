(ns raytracer.shapes.shared
  (:require [raytracer.point :as point]
            [raytracer.svector :as svector]))

(defprotocol Intersectable
  (local-intersect [this ray]))

(defprotocol Surface
  (compute-normal [this point]))

(defprotocol Hierarchy
  (add [this shape])
  (get-parent [this]))

(defn as-point [v]
  (point/point (:x v) (:y v) (:z v)))

(defn as-vector [p]
  (svector/svector (:x p) (:y p) (:z p)))


