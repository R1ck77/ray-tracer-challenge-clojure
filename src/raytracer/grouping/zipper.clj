(ns raytracer.grouping.zipper
  (:require [clojure.zip :as z]
            [raytracer.matrix :as matrix]
            [raytracer.grouping.shared :as shared]
            [raytracer.shapes.group :as group])
  (:import [raytracer.shapes.group Group]))

;;; TODO/FIXME Reference implementation: remove me once I have a better one
(defn temp-all-nodes [zipper]
  (println (z/node zipper))
  (let [next (z/next zipper)]    
    (when (not (z/end? next))
      (recur next))))

;;; TODO/FIXME wrong order of the results. Should return the first element 
(defn- next-matching-zipper [zipper predicate]
  (loop [zipper zipper]
    (if (z/end? zipper)
      nil
      (let [next (z/next zipper)]
        (if (predicate (z/node next))
          next
          (recur next))))))

(defn- lazy-get-all-matching-objects [zipper predicate]
  (let [next-zipper (next-matching-zipper zipper predicate)]
    (if next-zipper
     (cons (z/node next-zipper)
           (lazy-seq (lazy-get-all-matching-objects next-zipper predicate)))
     nil)))

(defn- get-all-matching-objects [zipper predicate]
  (loop [objects #{}
         zipper zipper]
    (if (z/end? zipper)
      objects
      (let [next (z/next zipper)
            next-node (z/node next)]
        (recur (if (predicate next-node)
                 (conj objects next-node)
                 objects)
               next)))))

(defn- get-all-non-group-objects [zipper]
  (get-all-matching-objects zipper
                            (complement
                             #(instance? Group %))))

(defn- do-find-node
  ([zipper predicate]
   (let [next (z/next zipper)
         current-node (z/node next)]
     (if (predicate current-node)
       next     
       (if (z/end? next)
         nil
         (recur next predicate))))))

(defn- do-compute-local-to-world-transform  [zipper shape]
  (reduce #(matrix/mul4 %2 %)
          (reverse
           (map :inverse-transposed-transform
                (conj (z/path (do-find-node zipper #(= shape %)))
                      shape)))))

(defn- do-compute-world-to-local-transform  [zipper shape]
  (reduce #(matrix/mul4 %2 %)
          (map :inverse-transform
               (conj (z/path (do-find-node zipper #(= shape %)))
                     shape))))

(defn- branch? [node]
  (instance? Group node))

(defn- get-children [node]
  (seq (:children node)))

(defn- new-node [node children]
  (assoc node :children (vec children)))

(defprotocol GroupZipper
  (compute-local-to-world-transform [this shape])
  (compute-world-to-local-transform [this shape])
  (find-node [this predicate]))

(defn- new-group-zipper [root]
  (z/zipper branch?
              get-children
              new-node
              root))

(defn create-zipper [root]
  (let [zipper (new-group-zipper root)]
    (reify
      GroupZipper
      (compute-local-to-world-transform [this shape]
        (do-compute-local-to-world-transform zipper shape))
      (compute-world-to-local-transform [this shape]
        (do-compute-world-to-local-transform zipper shape))
      (find-node [this predicate]
        (do-find-node zipper predicate))
      shared/ShapesContainer
      (get-root [this]
        root)
      (get-all-objects [this]
        (get-all-non-group-objects zipper)))))
