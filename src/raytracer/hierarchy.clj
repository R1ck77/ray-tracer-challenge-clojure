(ns raytracer.hierarchy
  (:require [clojure.zip :as zip]
            [raytracer.shapes.group :as group]
            [raytracer.shapes :as shapes]
            [raytracer.matrix :as matrix])
  (:import [raytracer.shapes.group Group]))


(defprotocol CoordinatesConverter
  (to-world-coordinates [this shape point]))

(defn find-node
  ([zipper predicate]
   (let [next (zip/next zipper)
         current-node (zip/node next)]
     (if (predicate current-node)
       next     
       (if (zip/end? next)
         nil
         (recur next predicate))))))

(defn- get-transforms [zipper object]
  (map :inverse-transpose-transform
       (conj (zip/path (find-node zipper #(= object %)))
             object)))

(defrecord Hierarchy [zipper]
  CoordinatesConverter
  (to-world-coordinates [this shape point]  ;;; TODO/FIXME totally bogus
    (reduce (fn transform [point matrix]  ;;; premultiply all of them
              (matrix/transform point matrix))
            point
            (get-transforms zipper shape)))) ;;; memoize

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
