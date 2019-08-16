(ns raytracer.shapes.optimizers.bisection-recursive
  (:require [raytracer.shapes.optimizers.optimizer :as optimizer]
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


(defn- sort-shapes [shapes predicate positive-key negative-key]
  (reduce (fn sort-shape [acc shape]
            (update acc
                    (if (predicate shape)
                      positive-key
                      negative-key)
                    #(conj % shape)))
          {positive-key []
           negative-key []}
          shapes))

(defn- sort-infinite-shapes [shapes]
  (sort-shapes shapes is-infinite? :infinite :finite))

(defn- bisect-recursively [group max-size]
  (println "* Warning: optimization not yet implemented!")
  group)

(defn create [max-size]
  (reify optimizer/GroupOptimizer
    (optimize [this group]
      (bisect-recursively group max-size))))
