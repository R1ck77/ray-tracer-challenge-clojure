(ns raytracer.shapes.csg
  (:require [raytracer.intersection :as intersection]
            [raytracer.shapes.shared :as shared]))

(defprotocol CSG
  (is-intersection-allowed? [this left-shape-hit inside-left-shape inside-right-shape])
  (filter-intersections [this xintersection]))

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

(defrecord CSGUnion [left-shape right-shape]
  CSG
  (is-intersection-allowed? [this left-shape-hit inside-left-shape inside-right-shape]
    (or (and left-shape-hit (not inside-right-shape))
        (and (not left-shape-hit) (not inside-left-shape))))
  (filter-intersections [this intersections]
    (discard-intersections this intersections)))

(defn union [left-shape right-shape]
  (->CSGUnion left-shape right-shape))

(defrecord CSGIntersection [left-shape right-shape]
  CSG
  (is-intersection-allowed? [this left-shape-hit inside-left-shape inside-right-shape]
    (or (and left-shape-hit inside-right-shape)
        (and (not left-shape-hit) inside-left-shape)))
  (filter-intersections [this intersections]
    (discard-intersections this intersections)))

(defn intersection [left-shape right-shape]
  (->CSGIntersection left-shape right-shape))

(defrecord CSGDifference [left-shape right-shape]
  CSG
  (is-intersection-allowed? [this left-shape-hit inside-left-shape inside-right-shape]
    (or (and left-shape-hit (not inside-right-shape))
        (and (not left-shape-hit) inside-left-shape)))
  (filter-intersections [this intersections]
    (discard-intersections this intersections)))

(defn difference [left-shape right-shape]
  (->CSGDifference left-shape right-shape))
