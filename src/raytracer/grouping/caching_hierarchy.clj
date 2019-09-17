(ns raytracer.grouping.caching-hierarchy
  (:require [raytracer.tuple :as tuple]
            [raytracer.grouping.hierarchy :as hierarchy]
            [raytracer.shapes.shared :as shared]            
            [raytracer.matrix :as matrix]
            [raytracer.grouping.zipper :as zipper]
            [raytracer.grouping.shared :as grouping-shared]))

(defn- create-memoized-world-to-local [zipper]
  (memoize (partial zipper/compute-world-to-local-transform zipper)))

(defn- create-memoized-local-to-world [zipper]
  (memoize (partial zipper/compute-local-to-world-transform zipper)))

(def from-zipper)

(defrecord CachingHierarchy [group-zipper root all-objects memoized-local-to-world memoized-world-to-local]
  grouping-shared/CoordinatesConverter  
  (local-to-world-coordinates [this shape svector]  
    (tuple/normalize
     (shared/as-vector
      (matrix/transform
       (memoized-local-to-world shape) svector))))
  (world-to-local-coordinates [this shape point]
    (matrix/transform
     (memoized-world-to-local shape) point))
  grouping-shared/ShapesContainer
  (get-root [this] root)
  (get-all-objects [this] all-objects)
  grouping-shared/HierarchyEditor
  (add-root-object [this object]
    (from-zipper (grouping-shared/add-root-object group-zipper object))))

(defn- from-zipper [zipper]
  (->CachingHierarchy zipper
                      (grouping-shared/get-root zipper)
                      (grouping-shared/get-all-objects zipper)
                      (create-memoized-local-to-world zipper)
                      (create-memoized-world-to-local zipper)))

(defn caching-hierarchy [group]
  (from-zipper (zipper/create-zipper group)))
