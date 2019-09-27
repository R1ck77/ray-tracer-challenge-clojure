(ns raytracer.grouping.zipper
  (:require [raytracer.matrix :as matrix]
            [raytracer.grouping.shared :as shared]
            [raytracer.shapes.shared :as shapes-shared]
            [raytracer.shapes.group :as group]
            [raytracer.shapes.placement :as placement]
            [raytracer.shapes.group.base-zipper :as base-zipper])
  (:import [raytracer.shapes.group Group EmptyGroup]))

(defn- do-compute-local-to-world-transform  [zipper shape]
  (reduce #(matrix/mul4 %2 %)
          (reverse
           (map #(-> %
                     shapes-shared/get-placement
                     placement/get-inverse-transposed-transform)
                (conj (base-zipper/path-to-node zipper #(= shape %))
                      shape)))))

(defn- do-compute-world-to-local-transform  [zipper shape]
  (reduce #(matrix/mul4 %2 %)
          (map #(-> %
                    shapes-shared/get-placement
                    placement/get-inverse-transform)
               (conj (base-zipper/path-to-node zipper #(= shape %))
                     shape))))

(defprotocol GroupZipper
  (compute-local-to-world-transform [this shape])
  (compute-world-to-local-transform [this shape])
  (find-node [this predicate]))

(defn create-zipper
  ([root]
   (create-zipper root (base-zipper/new-group-zipper root)))
  ([root zipper]
   (reify
     GroupZipper
     (compute-local-to-world-transform [this shape]
       (do-compute-local-to-world-transform zipper shape))
     (compute-world-to-local-transform [this shape]
       (do-compute-world-to-local-transform zipper shape))
     (find-node [this predicate]
       (base-zipper/find-node zipper predicate))
     shared/ShapesContainer
     (get-root [this]
       root)
     (get-all-objects [this]
       (base-zipper/get-all-non-group-objects zipper))
     shared/HierarchyEditor
     (base-zipper/add-root-object [this object]
       (create-zipper root
                      (base-zipper/add-root-obj zipper object)))
     (update-objects [this update-f]
       (create-zipper
        (base-zipper/update-zipper zipper update-f))))))
