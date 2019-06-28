(ns raytracer.hierarchy
  (:require [clojure.zip :as zip]
            [raytracer.tuple :as tuple]
            [raytracer.shapes.group :as group]
            [raytracer.shapes :as shapes]
            [raytracer.shapes.shared :as shared]            
            [raytracer.matrix :as matrix])
  (:import [raytracer.shapes.group Group]))


(defprotocol CoordinatesConverter
  (local-to-world-coordinates [this shape svector])
  (world-to-local-coordinates [this shape point]))

(defn find-node
  ([zipper predicate]
   (let [next (zip/next zipper)
         current-node (zip/node next)]
     (if (predicate current-node)
       next     
       (if (zip/end? next)
         nil
         (recur next predicate))))))

(defn- get-local-to-world-transforms  [zipper object]
  (reverse
   (map :inverse-transposed-transform
        (conj (zip/path (find-node zipper #(= object %)))
              object))))

(defn- get-world-to-local-transform  [zipper object]
  (reduce #(matrix/mul4 %2 %)
          (map :inverse-transform
               (conj (zip/path (find-node zipper #(= object %)))
                     object))))

;;; TODO/FIXME I suspect I'm using the wrong matrix
;;; TODO/FIXME totally bogus: premultiply and cache the results
(defrecord Hierarchy [zipper]
  CoordinatesConverter  
  (local-to-world-coordinates [this shape svector]  
    (tuple/normalize
     (reduce (fn transform-vector [svector matrix]
               (shared/as-vector (matrix/transform matrix svector)))
             svector
             (get-local-to-world-transforms zipper shape))))
  (world-to-local-coordinates [this shape point]
    (matrix/transform (get-world-to-local-transform zipper shape) point)))

(defn- branch? [node]
  (instance? Group node))

(defn- get-children [node]
  (:children node))

(defn- new-node [node children]
  (assoc node :children (vec children)))

(defn- create-zipper [root]
  (zip/zipper branch?
              get-children
              new-node
              root))

(defn hierarchy
  ([]
   (hierarchy (group/group [])))
  ([root]
   (->Hierarchy (create-zipper root))))
