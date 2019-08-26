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

(defn- quick-invert [m]
  (if (= m matrix/identity-matrix)
    matrix/identity-matrix
    (matrix/invert m 4)))

(defn- quick-transpose [m]
  (if (= m matrix/identity-matrix)
    matrix/identity-matrix
    (matrix/transpose m)))

(defn placement
  ([] (placement matrix/identity-matrix))
  ([transform-matrix]
   (let [inverse (quick-invert transform-matrix)]
     (->Placement transform-matrix
                  inverse
                  (quick-transpose inverse)))))

(defn- basic-change-transform [a-placement new-transform]
  (if (= (:transform a-placement)
         new-transform)
    a-placement
    (placement new-transform)))

(defn change-shape-transform [shape transform]
  (update shape :placement #(shared/change-transform % transform)))
