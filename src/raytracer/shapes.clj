(ns raytracer.shapes
  (:require [raytracer.utils :as utils]
            [raytracer.matrix :as matrix]
            [raytracer.shapes.shared :as shared]
            [raytracer.shapes.sphere :as sphere-ns]))



(defn same-shape? [a b]
  (if (= (merge a {:normal nil})
         (merge b {:normal nil}))
    a
    nil))


(defn change-transform [shape new-transform]
  (shared/add-normal-f (merge shape {:transform new-transform
                                     :inverse-transform (matrix/invert new-transform 4)})))

(defn change-material [shape new-material]
  (assoc shape :material new-material))

(defn sphere []
  (sphere-ns/sphere))
