(ns raytracer.hierarchy
  (:require [clojure.zip :as zip]
            [raytracer.shapes.group :as group]
            [raytracer.shapes :as shapes])
  (:import [raytracer.shapes.group Group]))

(defprotocol CoordinatesConverter
  (to-world-coordinates [this shape point])
  (get-parents [this object]))

(defrecord Hierarchy [zipper]
  CoordinatesConverter
  (to-world-coordinates [this shape point])
  (get-parents [this object]))

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

(defn all-nodes
  ([zipper]
   (all-nodes zipper []))
  ([zipper acc]
   (let [next (zip/next zipper)
         acc (conj acc (zip/node next))]
     (if (zip/end? next)
       acc
       (recur next acc)))))

(defn hierarchy
  ([]
   (hierarchy (group/group [])))
  ([root]
   (->Hierarchy (create-zipper root))))

(defn test-branch? [node]
  (println "Test branch on " node)
  (vector? node))

(defn test-children [node]
  (println "Test get children on " node)
  node)

(defn test-new-branch [node children]
  (println "Test new branch on " node "and" children)
  (vec children))

(defn test-zipper [root]
  (println "Creating a zipper from " root)
  (zip/zipper test-branch?
              test-children
              test-new-branch
              root))

(defn test-all-nodes
  ([zipper]
   (test-all-nodes [] (zip/next zipper)))
  ([acc zipper]
   (let [new-acc (conj acc (zip/node zipper))]
     (if (zip/end? zipper)
       new-acc
       (test-all-nodes new-acc (zip/next zipper))))))

