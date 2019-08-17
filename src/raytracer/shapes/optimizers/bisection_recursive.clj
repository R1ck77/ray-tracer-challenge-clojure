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

;;;; STUFF TO MOVE ABOVE THIS POINT

(defn- debug-print-intermediate [{:keys [sub-boxes, outliers] :as result}]
  (println "Results: " (map count (vals sub-boxes)) "Outliers: " (count outliers))
  result)

(def partition-shapes)

(defn- create-groups [{:keys [sub-boxes, outliers]} max-size]
  (vec (map group/group (conj (map #(partition-shapes % max-size) (vals sub-boxes)) outliers))))

(defn- assign-shape [{:keys [sub-boxes, outliers]} shape]
  (let [boxes (keys sub-boxes)
        assigned (first (filter #(box/contains % shape) boxes))]
    (if assigned
      ;; hic sunt leones. Assign the shape to the right box
      {:outliers outliers, :sub-boxes (update sub-boxes assigned #(conj % shape))}
      {:sub-boxes sub-boxes, :outliers (conj outliers shape)})))

(defn- groups-to-map
  "Create a map of groups vs shapes assigned to it"
  [set]
  (into {} (map #(vector % []) set)))

(defn- compute-extremes
  "Return the size of a bounding box containing all shapes"
  [shapes]
  (bounding-box/get-corners (group/group shapes)))

(defn- split-shapes [shapes max-size]
  (let [main-box (apply box/box (compute-extremes shapes))
        sub-boxes (box/bisect main-box)]
    (create-groups
     (reduce assign-shape {:sub-boxes (groups-to-map sub-boxes)
                           :outliers []} shapes)
     max-size)))

(defn- partition-shapes
  "Recursively partition each group into at most max-size objects, returns a list of shapes"
  [shapes max-size]
  (if (not (zero? (count shapes)))
    (println (format "%d shapes (vs %d)" (count shapes) max-size)))
  (if (<= (count shapes) max-size)
    shapes
    (split-shapes shapes max-size)))

(def bisect-recursively)

(defn- partition-children
  "Split the children in sub-groups if the "
  [shapes max-size]
  (map #(if (satisfies? group/Parent %)
          (bisect-recursively % max-size)
          %) shapes))

(defn- bisect-recursively
  "Create a spatially balanced tree of groups of shapes

  Infinite shapes are ignored and kept in a special group, the other are split in some way"
  [group max-size]
  {:pre [(satisfies? group/Parent group)]}
  (println (format "Group of %d children" (count (:children group))))
  (group/set-children group (-> group
                                :children
                                (partition-children max-size)
                                (partition-shapes max-size))))

(defn create [max-size]
  (reify optimizer/GroupOptimizer
    (optimize [this group]
      (bisect-recursively group max-size))))
