(ns raytracer.shapes
  (:require [raytracer.utils :as utils]
            [raytracer.matrix :as matrix]
            [raytracer.shapes.shared :as shared]
            [raytracer.shapes.sphere :as sphere-ns]
            [raytracer.shapes.plane :as plane-ns]))

(defn change-transform [shape new-transform]
  (merge shape {:transform new-transform
                :inverse-transform (matrix/invert new-transform 4)}))

(defn change-material [shape new-material]
  (assoc shape :material new-material))

(defn sphere []
  (sphere-ns/sphere))

(defn plane []
  (plane-ns/plane))
