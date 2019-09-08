(ns raytracer.shapes.csg
  (:require [raytracer.intersection :as intersection]
            [raytracer.ray :as ray]
            [raytracer.shapes.placement :as placement]
            [raytracer.shapes.shared :as shared]))

(defprotocol CSG
  (is-intersection-allowed? [this left-shape-hit inside-left-shape inside-right-shape])
  (filter-intersections [this xintersection]))

(defn- intersect-component [ray-object-space shape]
  (shared/local-intersect shape
                          (ray/transform ray-object-space
                                         (-> shape :placement placement/get-inverse-transform))))


(defn- local-intersect [csg-shape ray-object-space]
  (let [intersect (partial intersect-component ray-object-space)]
   (filter-intersections csg-shape
                         (sort-by :t (concat (intersect (:left-shape csg-shape))
                                             (intersect (:right-shape csg-shape)))))))

(defn- csg-includes? [csg-shape object]
  (or (identical? object csg-shape)
      (identical? object (:left-shape csg-shape))
      (identical? object (:right-shape csg-shape))))

(defn- create-intersection-data [intersection is-left-object inside-left inside-right]
  (list intersection
        is-left-object
        inside-left
        inside-right))

(defn- update-status [state intersection is-left-object]
  (update state
          (if is-left-object
            :inside-left
            :inside-right)
          #(not %)))

(defn- categorize-intersection [state intersection left-object intersection-object]
  (let [is-left-object (shared/includes? left-object intersection-object)]
    (update (update-status state intersection is-left-object)
            :intersections-data
            (fn [intersections]
              (conj intersections (create-intersection-data intersection
                                                            is-left-object
                                                            (:inside-left state)
                                                            (:inside-right state)))))))

(defn- categorize-intersections [intersections left-object right-object]
  (:intersections-data
   (reduce #(categorize-intersection % %2 left-object (:object %2))
           {:inside-left false
            :inside-right false
            :intersections-data []}
           intersections)))

(defn- discard-intersections [this intersections]
  (map first
       (filter (fn [[_ is-left-object inside-left inside-right]]
                 (is-intersection-allowed? this is-left-object inside-left inside-right))
               (categorize-intersections intersections (:left-shape this) (:right-shape this)))))

(defrecord CSGUnion [left-shape right-shape placement]
  CSG
  (is-intersection-allowed? [this left-shape-hit inside-left-shape inside-right-shape]
    (or (and left-shape-hit (not inside-right-shape))
        (and (not left-shape-hit) (not inside-left-shape))))
  (filter-intersections [this intersections]
    (discard-intersections this intersections))
  shared/Container
  (includes? [this object]
    (csg-includes? this object))
  shared/Intersectable
  (local-intersect [this ray-object-space]
    (local-intersect this ray-object-space))
  shared/Transformable
  (change-transform [this transform-matrix]
    (placement/change-shape-transform this transform-matrix)))

(defn union [left-shape right-shape]
  (->CSGUnion left-shape right-shape (placement/placement)))

(defrecord CSGIntersection [left-shape right-shape placement]
  CSG
  (is-intersection-allowed? [this left-shape-hit inside-left-shape inside-right-shape]
    (or (and left-shape-hit inside-right-shape)
        (and (not left-shape-hit) inside-left-shape)))
  (filter-intersections [this intersections]
    (discard-intersections this intersections))
  shared/Container
  (includes? [this object]
    (csg-includes? this object))
  shared/Intersectable
  (local-intersect [this ray-object-space]
    (local-intersect this ray-object-space))
  shared/Transformable
  (change-transform [this transform-matrix]
    (placement/change-shape-transform this transform-matrix)))

(defn intersection [left-shape right-shape]
  (->CSGIntersection left-shape right-shape (placement/placement)))

(defrecord CSGDifference [left-shape right-shape placement]
  CSG
  (is-intersection-allowed? [this left-shape-hit inside-left-shape inside-right-shape]
    (or (and left-shape-hit (not inside-right-shape))
        (and (not left-shape-hit) inside-left-shape)))
  (filter-intersections [this intersections]
    (discard-intersections this intersections))
  shared/Container
  (includes? [this object]
    (csg-includes? this object))
  shared/Intersectable
  (local-intersect [this ray-object-space]
    (local-intersect this ray-object-space))
  shared/Transformable
  (change-transform [this transform-matrix]
    (placement/change-shape-transform this transform-matrix)))

(defn difference [left-shape right-shape]
  (->CSGDifference left-shape right-shape (placement/placement)))
