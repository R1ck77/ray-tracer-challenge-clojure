(ns raytracer.shapes.placement
  (:require [raytracer.matrix :as matrix]
            [raytracer.shapes.shared :as shared]))

(defprotocol PlacementProvider
  (get-transform [this])
  (get-inverse-transform [this])
  (get-inverse-transposed-transform [this]))

(def basic-change-transform)

(defrecord Placement [transform inverse-transform inverse-transposed-transform]
  shared/Transformable
  (change-transform [this new-transform]
    (basic-change-transform this new-transform))
  PlacementProvider
  (get-transform [this] transform)
  (get-inverse-transform [this] inverse-transform)
  (get-inverse-transposed-transform [this] inverse-transposed-transform))

(defn placement
  ([] (placement matrix/identity-matrix))
  ([transform-matrix]
   (let [inverse (matrix/invert transform-matrix 4)]
     (->Placement transform-matrix inverse (matrix/transpose inverse)))))

(defn basic-change-transform [_ new-transform]
  (placement new-transform))

(defn change-shape-transform [shape transform]
  (update shape :placement #(shared/change-transform % transform)))
