(ns raytracer.shapes
  (:require [raytracer.utils :as utils]
            [raytracer.matrix :as matrix]
            [raytracer.materials :as materials]
            [raytracer.shapes.shared :as shared]
            [raytracer.shapes.sphere :as sphere-ns]
            [raytracer.shapes.plane :as plane-ns]))

(defn change-transform [shape new-transform]
  (merge shape {:transform new-transform
                :inverse-transform (matrix/invert new-transform 4)}))

(defn change-material [shape new-material]
  (assoc shape :material new-material))

(defn update-material [shape material-update-f]
  (change-material shape (material-update-f (:material shape))))

(defn sphere []
  (sphere-ns/sphere))

(defn- glass-object [shape] ;;; TODO/FIXME is this a fixture and belongs to the tests?
  (change-material shape
                   (materials/update-material (:material shape)
                                              :transparency 1.0
                                              :refractive-index 1.5)))

(defn glass-sphere []
  (glass-object (sphere-ns/sphere)))

(defn plane []
  (plane-ns/plane))
