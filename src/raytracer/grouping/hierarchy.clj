(ns raytracer.grouping.hierarchy
  (:require [clojure.zip :as zip]
            [raytracer.tuple :as tuple]
            [raytracer.shapes.group :as group]
            [raytracer.shapes :as shapes]
            [raytracer.shapes.shared :as shared]            
            [raytracer.matrix :as matrix]
            [raytracer.grouping.zipper :as zipper]
            [raytracer.grouping.shared :as grouping-shared]))

(defrecord Hierarchy [group-zipper]
  grouping-shared/CoordinatesConverter  
  (local-to-world-coordinates [this shape svector]  
    (tuple/normalize
     (shared/as-vector
      (matrix/transform
       (zipper/compute-local-to-world-transform group-zipper shape) svector))))
  (world-to-local-coordinates [this shape point]
    (matrix/transform
     (zipper/compute-world-to-local-transform group-zipper shape) point))
  grouping-shared/ShapesContainer
  (get-root [this]
    (grouping-shared/get-root group-zipper))
  (get-all-objects [this]
    (grouping-shared/get-all-objects group-zipper))
  grouping-shared/HierarchyEditor
  (add-root-object [this object]
    (grouping-shared/add-root-object group-zipper object)))

(defn hierarchy
  ([]
   (hierarchy (group/group [])))
  ([root]
   (->Hierarchy (zipper/create-zipper root))))
