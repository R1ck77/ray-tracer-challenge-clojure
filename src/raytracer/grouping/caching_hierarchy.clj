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

(defrecord CachingHierarchy [root memoized-local-to-world memoized-world-to-local]
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
  (get-root [this]
    root))

(defn caching-hierarchy [group]
  (let [zipper (zipper/create-zipper group)]
    (->CachingHierarchy group
                        (create-memoized-local-to-world zipper)
                        (create-memoized-world-to-local zipper))))
