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
                                                                       (-> % :placement placement/get-inverse-transform)))
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

(defrecord Group [children placement aabb-extremes]
  shared/Transformable
  (change-transform [this transform-matrix]
    (let [new-aabb-extremes (compute-extremes transform-matrix (:children this))]
     (assoc (placement/change-shape-transform this transform-matrix)
            :aabb-extremes new-aabb-extremes)))
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
  (get-corners [this] ;;; TODO/FIXME this *has* to be cached at object creation
    (compute-extremes (-> this :placement placement/get-transform) children))
  (get-transformed-extremes [this] ;;; TODO/FIXME and guess what? This too…
    (bounding-box/compute-filtered-transformed-extremes (bounding-box/get-corners this)
                                                        (-> this :placement placement/get-transform)))
  Parent
  (get-children [this]
    children)
  (is-empty? [this] false)
  (set-children [this new-children]
    (merge this {:children new-children
                 :aabb-extremes (compute-extremes (-> this :placement placement/get-transform) new-children)}))
  Optimizer
  (optimize [this optimizer]
    (optimizer/optimize optimizer this)))

(defrecord EmptyGroup [placement]
  shared/Transformable
  (change-transform [this transform-matrix]
    (placement/change-shape-transform this transform-matrix))  
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
    (->Group new-children
             (:placement this)
             (compute-extremes (-> this :placement placement/get-transform) new-children)))
  Optimizer
  (optimize [this optimizer] this))

(defn group [children]
  (if (empty? children)
    (->EmptyGroup (placement/placement matrix/identity-matrix))
    (->Group children
             (placement/placement matrix/identity-matrix)
             (compute-extremes matrix/identity-matrix children))))

(def empty-group (group []))
