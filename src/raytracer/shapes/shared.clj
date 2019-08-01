(ns raytracer.shapes.shared
  (:require [raytracer.point :as point]
            [raytracer.matrix :as matrix]
            [raytracer.svector :as svector]))

(defprotocol Intersectable
  (local-intersect [this ray]))

(defprotocol Surface
  (compute-normal [this point]))

(defprotocol Transformable
  (transform [this transform]))

(defn as-point [v]
  (point/point (:x v) (:y v) (:z v)))

(defn as-vector [p]
  (svector/svector (:x p) (:y p) (:z p)))

(defn change-transform [shape new-transform]
  (let [inverse (matrix/invert new-transform 4)]
    (merge shape {:transform new-transform
                  :inverse-transform inverse
                  :inverse-transposed-transform (matrix/transpose inverse)})))



