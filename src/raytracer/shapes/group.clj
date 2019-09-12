(ns raytracer.shapes.group
  (:require [raytracer.point :as point]
            [raytracer.matrix :as matrix]
            [raytracer.ray :as ray]
            [raytracer.shapes.bounding-box :as bounding-box]            
            [raytracer.shapes.aabb-intersection :as aabb-intersection]
            [raytracer.shapes.shared :as shared]
            [raytracer.shapes.optimizers.optimizer :as optimizer]
            [raytracer.intersection :as intersection]
            [raytracer.shapes.placement :as placement]))

(def ^:dynamic *use-bounding-boxes* true)
(def ^:dynamic *statistics* false) ; whether sampling the number of aabb hits or not

(def hit-count-statistics (atom [0 0])) ; used for debugging statistics only

(def group)

(defn- intersect [group ray-object-space]
  (let [transformed-ray (ray/transform ray-object-space (-> group :placement placement/get-inverse-transform))]
    (sort-by :t (apply concat
                        (map #(shared/local-intersect % (ray/transform transformed-ray
                                                                       (-> % shared/get-placement  placement/get-inverse-transform)))
                             (:children group))))))

(defn compute-extremes
  "This method makes testing a mess. I'm wiring something to accept non BoundingBox objects too

  (otherwise I get the worst of two world, strictly typed execution in a weakly typed language…)"
  [group-transform children]
  (let [corners (mapcat (fn [children]
                          (if (satisfies? bounding-box/BoundingBox children)
                            (bounding-box/get-transformed-extremes children)
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

(defprotocol Optimizer
  (optimize [this optimizer] "Return a new group optimized with a specific optimizer"))

(defrecord Group [children placement aabb-extremes transformed-extremes]
  shared/Transformable
  (change-transform [this transform-matrix]
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
  shared/Surface
  (compute-normal [this point]
    (throw (UnsupportedOperationException. "Normal of group computed")))
  (compute-normal [this point _]
    (throw (UnsupportedOperationException. "Normal of group computed")))
  bounding-box/BoundingBox
  (hit [this ray]
    (let [result (aabb-intersection/hit aabb-extremes ray)]
      (update-statistics result)
      result))
  (get-corners [this]
    aabb-extremes)
  (get-transformed-extremes [this]
    transformed-extremes)
  Parent
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
    (set-children this (map #(shared/change-material % new-material) children)))
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
  bounding-box/BoundingBox
  (hit [this ray] false)
  (get-corners [this]
    [(point/point 0 0 0) (point/point 0 0 0)])
  (get-transformed-extremes [this]
    [(point/point 0 0 0) (point/point 0 0 0)])
  Parent
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
