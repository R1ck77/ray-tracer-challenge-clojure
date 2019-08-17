(ns raytracer.shapes.optimizers.bisection-recursive
  (:require [raytracer.shapes.optimizers.optimizer :as optimizer]
            [raytracer.shapes.optimizers.box :as box]
            [raytracer.const :as const]
            [raytracer.shapes.bounding-box :as bounding-box]
            [raytracer.shapes.group :as group]))

(defn- is-infinite-point? [{x :x, y :y, z :z}]
  (or (>= (Math/abs (float x)) const/inf)
      (>= (Math/abs (float y)) const/inf)
      (>= (Math/abs (float z)) const/inf)))

(defn- compute-extremes
  "Return the size of a bounding box containing all shapes"
  [shapes]
  (bounding-box/get-corners (group/group shapes)))

(def partition-shapes)

(defn- groups-to-map
  "Create a map of groups vs shapes assigned to it"
  [set]
  (into {} (map #(vector % []) set)))

(defn- assign-shape [{:keys [sub-boxes, outliers]} shape]
  (let [boxes (keys sub-boxes)
        assigned (filter #(box/contains % shape) boxes)]
    (if (empty? assigned)
      {:sub-boxes sub-boxes, :outliers (conj outliers shape)}
      ;; hic sunt leones. Assign the shape to the right box
      )))

(defn- split-shapes [shapes max-size]
  (let [main-box (box/box (compute-extremes shapes))
        sub-boxes (box/bisect main-box)]
    (reduce assign-shape {:sub-boxes (groups-to-map sub-boxes)
                          :outliers []})))


(defn- partition-shapes
  "Recursively partition each group into at most max-size objects"
  [shapes max-size]
  (if (<= (count shapes) max-size)
    (group/group shapes)
    (split-shapes shapes max-size))
  
  )

(defn is-infinite?
  "Returns true if the extension of a shape is infinite.

Infinites are not composed, so if any point of the shape is infinite, the shape will be"
  [shape]
  (not (empty?
        (filter is-infinite-point? (bounding-box/get-transformed-points shape)))))


(defn- sort-shapes
  "sort shapes and put them in a dictionary depending how they are classified by the predicate

  Shapes passing the test go in a vector under 'positive-key' and the other in a vector in 'negative-key'"
  [shapes predicate positive-key negative-key]
  (reduce (fn sort-shape [acc shape]
            (update acc
                    (if (predicate shape)
                      positive-key
                      negative-key)
                    #(conj % shape)))
          {positive-key []
           negative-key []}
          shapes))

(defn- sort-infinite-shapes
  "sort shapes in a dictionary {:finite [], :infinite []} depending on the size"
  [shapes]
  (sort-shapes shapes is-infinite? :infinite :finite))

(defn- bisect-recursively
  "Create a spatially balanced tree of groups of shapes

  Infinite shapes are ignored and kept in a special group, the other are split in some way"
  [group max-size]
  (let [{finite :finite, infinite :infinite} (sort-infinite-shapes (:children group))]
    (group/group (conj (partition-shapes finite max-size)
                       (group/group infinite)))))

(defn create [max-size]
  (reify optimizer/GroupOptimizer
    (optimize [this group]
      (bisect-recursively group max-size))))
