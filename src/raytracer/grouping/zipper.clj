(ns raytracer.grouping.zipper
  (:require [clojure.zip :as zip]
            [raytracer.matrix :as matrix])
  (:import [raytracer.shapes.group Group]))

(defn- do-find-node
  ([zipper predicate]
   (let [next (zip/next zipper)
         current-node (zip/node next)]
     (if (predicate current-node)
       next     
       (if (zip/end? next)
         nil
         (recur next predicate))))))

(defn- do-compute-local-to-world-transform  [zipper shape]
  (reduce #(matrix/mul4 %2 %)
          (reverse
           (map :inverse-transposed-transform
                (conj (zip/path (do-find-node zipper #(= shape %)))
                      shape)))))

(defn- do-compute-world-to-local-transform  [zipper shape]
  (reduce #(matrix/mul4 %2 %)
          (map :inverse-transform
               (conj (zip/path (do-find-node zipper #(= shape %)))
                     shape))))

(defn- branch? [node]
  (instance? Group node))

(defn- get-children [node]
  (:children node))

(defn- new-node [node children]
  (assoc node :children (vec children)))

(defprotocol GroupZipper
  (compute-local-to-world-transform [this shape])
  (compute-world-to-local-transform [this shape])
  (find-node [this predicate]))

(defn create-zipper [root]
  (let [zipper (zip/zipper branch?
               get-children
               new-node
               root)]
    (reify GroupZipper
      (compute-local-to-world-transform [this shape]
        (do-compute-local-to-world-transform zipper shape))
      (compute-world-to-local-transform [this shape]
        (do-compute-world-to-local-transform zipper shape))
      (find-node [this predicate]
        (do-find-node zipper predicate)))))
