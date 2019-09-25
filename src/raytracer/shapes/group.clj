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

(def group)

(defn- intersect [group ray-object-space]
  (sort-by :t (apply concat
                     (map #(shared/local-intersect % (ray/transform ray-object-space
                                                                    (-> % shared/get-placement placement/get-inverse-transform)))
                          (:children group)))))

(defprotocol Optimizer
  (optimize [this optimizer] "Return a new group optimized with a specific optimizer"))

(defrecord Group [children placement bounding-box]
  shared/Transformable
  (change-transform [this transform-matrix]
    (group children (placement/placement transform-matrix)))
  (get-placement [this] (:placement this))
  shared/Intersectable
  (local-intersect [this ray-object-space]
    (intersect this ray-object-space))
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
    (group new-children placement))
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
    (group [] (placement/placement transform-matrix)))  
  (get-placement [this] (:placement this))
  shared/Intersectable
  (local-intersect [this ray-object-space]
    [])
  (get-bounding-box [this]
    bounding-box/invisible-box)
  parent/Parent
  (get-children [this]
    [])
  (is-empty? [this] true)
  (set-children [this new-children]
    (group new-children placement))
  Optimizer
  (optimize [this optimizer] this)
  shared/Material
  (change-material [this new-material] this)
  (get-material [this]
    (throw (UnsupportedOperationException. "Group's material requested")))
  shared/Container
  (includes? [this object]
    (identical? this object)))

(defn- compute-bounding-box [children]
  (reduce bounding-box/merge (map shared/get-bounding-box children)))

(defn- create-non-empty-group [children placement]
  (->Group children placement
           (bounding-box/transform (compute-bounding-box children)
                                   (placement/get-transform placement))))

(defn group
  ([children]
   (group children (placement/placement matrix/identity-matrix)))
  ([children placement]
   {:pre [(every? (complement nil?) children)]}
   (if (empty? children)
     (->EmptyGroup placement)
     (create-non-empty-group children placement))))

(def empty-group (group []))
