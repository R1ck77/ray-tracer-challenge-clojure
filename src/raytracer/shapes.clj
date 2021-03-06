(ns raytracer.shapes
  (:require [raytracer.utils :as utils]
            [raytracer.matrix :as matrix]
            [raytracer.material :as material]
            [raytracer.shapes.shared :as shared]
            [raytracer.shapes.sphere :as sphere-ns]
            [raytracer.shapes.plane :as plane-ns]
            [raytracer.shapes.cube :as cube-ns]
            [raytracer.shapes.cylinder :as cylinder-ns]
            [raytracer.shapes.cone :as cone-ns]
            [raytracer.shapes.group :as group-ns]))

(defn update-material [shape material-update-f]
  (shared/change-material shape
                          (material-update-f (shared/get-material shape))))

(defn sphere [& args]
  (apply sphere-ns/sphere args))

(defn plane []
  (plane-ns/plane))

(defn cube []
  (cube-ns/cube))

(defn cylinder [& args]
  (apply cylinder-ns/cylinder args))

(defn cone [& args]
  (apply cone-ns/cone args))

(defn group [children]
  (group-ns/group children))
