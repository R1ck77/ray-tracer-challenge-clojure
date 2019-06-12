(ns raytracer.shapes
  (:require [raytracer.utils :as utils]
            [raytracer.matrix :as matrix]
            [raytracer.material :as material]
            [raytracer.shapes.shared :as shared]
            [raytracer.shapes.sphere :as sphere-ns]
            [raytracer.shapes.plane :as plane-ns]
            [raytracer.shapes.cube :as cube-ns]
            [raytracer.shapes.cylinder :as cylinder-ns]
            [raytracer.shapes.cone :as cone-ns]))

(defn change-transform [shape new-transform]
  (merge shape {:transform new-transform
                :inverse-transform (matrix/invert new-transform 4)}))

(defn change-material [shape new-material]
  (assoc shape :material new-material))

(defn update-material [shape material-update-f]
  (change-material shape (material-update-f (:material shape))))

(defn sphere []
  (sphere-ns/sphere))

(defn plane []
  (plane-ns/plane))

(defn cube []
  (cube-ns/cube))

(defn cylinder []
  (cylinder-ns/cylinder))

(defn cone []
  (cone-ns/cone))
