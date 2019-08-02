(ns raytracer.shapes.group
  ;;; TODO/FIXME check require
  (:require [raytracer.const :as const]
            [raytracer.point :as point]
            [raytracer.tuple :as tuple]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]
            [raytracer.ray :as ray]
            [raytracer.shapes.bounding-box :as bounding-box]            
            [raytracer.shapes.aabb-intersection :as aabb-intersection]
            [raytracer.shapes.shared :as shared]
            [raytracer.material :as material]
            [raytracer.intersection :as intersection]))

(def ^:dynamic *use-bounding-boxes* true)
(def ^:dynamic *statistics* false) ; whether sampling the number of aabb hits or not

(def hit-count-statistics (atom [0 0])) ; used for debugging statistics only

(def group)

(defn- intersect [group ray-object-space]
  (let [transformed-ray (ray/transform ray-object-space (:inverse-transform group))]
    (sort-by :t (reduce concat
                        (map #(shared/local-intersect % (ray/transform transformed-ray
                                                                       (:inverse-transform %)))
                             (:children group))))))

(defn compute-extremes
  "This method makes testing a mess. I'm wiring something to accept non BoundingBox objects too

  (otherwise I get the worst of two world, strictly typed execution in a weakly typed language…)"
  [group-transform children]
  (let [corners (mapcat (fn [children]
                          (if (satisfies? bounding-box/BoundingBox children)
                            (bounding-box/get-transformed-points children)
                            [])) children)]
    (if (empty? corners)
      [(point/point 0 0 0) (point/point 0 0 0)]
      (bounding-box/transform-extremes (bounding-box/extremes-from-points corners)
                                       group-transform))))

(defprotocol Parent
  (get-children [this])
  (is-empty? [this])
  (set-children [this children]))

(defn- update-statistics
  "Only used to debug the aabb statistics"
  [hit]
  (if *statistics*
    (swap! hit-count-statistics
           #(vector (if hit (inc (first %)) (first %))
                    (inc (second %))))))

(defn- bounding-box-check [group ray]
  (or (not *use-bounding-boxes*)
      (bounding-box/hit group ray)))

;;; TODO/FIXME the number of operations to do when you change the transform are
;;; starting to pile up
(defrecord Group [children transform inverse-transform inverse-transposed-transform aabb-extremes] ;;; TODO/FIXME this is really effed up
  shared/Transformable
  (transform [this transform-matrix]
    (assoc (shared/change-transform this transform-matrix)
           :aabb-extremes (compute-extremes transform-matrix (:children this))))
  shared/Intersectable
  (local-intersect [this ray-object-space]
    (if (bounding-box-check this ray-object-space)
      (intersect this ray-object-space)
      []))
  shared/Surface
  (compute-normal [this point]
    (throw (UnsupportedOperationException. "Normal of group computed")))
  bounding-box/BoundingBox
  (hit [this ray]
    (let [result (aabb-intersection/hit aabb-extremes ray)]
      (update-statistics result)
      result))
  (get-corners [this] ;;; TODO/FIXME this *has* to be cached at object creation
    (compute-extremes transform children))
  (get-transformed-points [this] ;;; TODO/FIXME and guess what? This too…
    (bounding-box/compute-filtered-transformed-extremes (bounding-box/get-corners this)
                                                        (:transform this)))
  Parent
  (get-children [this]
    children)
  (is-empty? [this] false)
  (set-children [this new-children]
    (merge this {:children new-children
                 :aabb-extremes (compute-extremes new-children)})))

(defrecord EmptyGroup [transform inverse-transform inverse-transposed-transform]
  shared/Transformable
  (transform [this transform-matrix]
    (shared/change-transform this transform-matrix))
  shared/Intersectable
  (local-intersect [this ray-object-space]
    [])
  bounding-box/BoundingBox
  (hit [this ray] false)
  (get-corners [this]
    [(point/point 0 0 0) (point/point 0 0 0)])
  (get-transformed-points [this]
    [(point/point 0 0 0) (point/point 0 0 0)])
  Parent
  (get-children [this]
    [])
  (is-empty? [this] true)
  (set-children [this new-children]
    (->Group new-children
             (:transform this)
             (:inverse-transform this)
             (:inverse-transposed-transform this)
             (compute-extremes new-children))))

(defn group [children]
  (if (empty? children)
    (->EmptyGroup matrix/identity-matrix
                  matrix/identity-matrix
                  matrix/identity-matrix)
    (->Group children
             matrix/identity-matrix
             matrix/identity-matrix
             matrix/identity-matrix
             (compute-extremes matrix/identity-matrix children))))



(def empty-group (group []))

