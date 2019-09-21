(ns raytracer.shapes.group
  (:require [raytracer.point :as point]
            [raytracer.matrix :as matrix]
            [raytracer.ray :as ray]
            [raytracer.shapes.bounding-box :as bounding-box]            
            [raytracer.shapes.aabb-intersection :as aabb-intersection]
            [raytracer.shapes.shared :as shared]
            [raytracer.shapes.optimizers.optimizer :as optimizer]
            [raytracer.intersection :as intersection]
            [raytracer.shapes.placement :as placement]
            [raytracer.shapes.parent :as parent]))

(def ^:dynamic *use-bounding-boxes* true)
(def ^:dynamic *statistics* false) ; whether sampling the number of aabb hits or not

(def hit-count-statistics (atom [0 0])) ; used for debugging statistics only

(def group)

(defn- intersect [group ray-object-space]
  (sort-by :t (apply concat
                     (map #(shared/local-intersect % (ray/transform ray-object-space
                                                                    (-> % shared/get-placement placement/get-inverse-transform)))
                          (:children group)))))

(defn- update-statistics
  "Only used to debug the aabb statistics"
  [hit]
  (if *statistics*
    (swap! hit-count-statistics
           #(vector (if hit (inc (first %)) (first %))
                    (inc (second %))))))

(defprotocol Optimizer
  (optimize [this optimizer] "Return a new group optimized with a specific optimizer"))

(defrecord Group [children placement transformed-extremes bounding-box]
  shared/Transformable
  (change-transform [this transform-matrix]
    (throw (UnsupportedOperationException. "Not done yet"))
    (let [new-aabb-extremes (compute-extremes transform-matrix (:children this))
          new-transformed-extremes (bounding-box/compute-filtered-transformed-extremes new-aabb-extremes transform-matrix)]
     (merge (placement/change-shape-transform this transform-matrix)
            {:aabb-extremes new-aabb-extremes
             :transformed-extremes new-transformed-extremes})))
  (get-placement [this] (:placement this))
  shared/Intersectable
  (local-intersect [this ray-object-space]
    (if (bounding-box-check this ray-object-space)
      (intersect this ray-object-space)
      []))
  (get-bounding-box [this]
    bounding-box)
  shared/Surface
  (compute-normal [this point hierarchy]
    (throw (UnsupportedOperationException. "Normal of group computed")))
  (compute-normal [this point _ hierarchy]
    (throw (UnsupportedOperationException. "Normal of group computed")))
  parent/Parent
  (get-children [this]
    children)
  (is-empty? [this] false)
  (set-children [this new-children]
    (merge this {:children new-children
                 :aabb-extremes (compute-extremes (-> this :placement placement/get-transform) new-children)}))
  Optimizer
  (optimize [this optimizer]
    (optimizer/optimize optimizer this))
  shared/Material
  (change-material [this new-material]
    (parent/set-children this (map #(shared/change-material % new-material) children)))
  (get-material [this]
    (throw (UnsupportedOperationException. "Group's material requested")))
  shared/Container
  (includes? [this object]
    (or (identical? this object)
        (some #(shared/includes? % object) children))))

(defrecord EmptyGroup [placement]
  shared/Transformable
  (change-transform [this transform-matrix]
    (placement/change-shape-transform this transform-matrix))  
  (get-placement [this] (:placement this))
  shared/Intersectable
  (local-intersect [this ray-object-space]
    [])
  (get-bounding-box [this]
    (bounding-box/->InvisibleBox)) ;; this should be a singleton reference
  parent/Parent
  (get-children [this]
    [])
  (is-empty? [this] true)
  (set-children [this new-children]
    (let [extremes (compute-extremes (-> this :placement placement/get-transform) new-children)]
      (->Group new-children
               (:placement this)
               extremes
               (bounding-box/compute-filtered-transformed-extremes extremes
                                                                   (-> this :placement placement/get-transform)))))
  Optimizer
  (optimize [this optimizer] this)
  shared/Material
  (change-material [this new-material] this)
  (get-material [this]
    (throw (UnsupportedOperationException. "Group's material requested")))
  shared/Container
  (includes? [this object]
    (identical? this object)))

(defn group [children]
  (if (empty? children)
    (->EmptyGroup (placement/placement matrix/identity-matrix))
    (let [extremes (compute-extremes matrix/identity-matrix children)]
      (->Group children
                     (placement/placement matrix/identity-matrix)
                     extremes
                     (bounding-box/compute-filtered-transformed-extremes extremes matrix/identity-matrix)))))

(def empty-group (group []))
