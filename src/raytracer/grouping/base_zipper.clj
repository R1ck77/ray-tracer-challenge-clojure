(ns raytracer.grouping.base-zipper
  (:require [clojure.zip :as z]
            [raytracer.shapes.parent :as parent]))

(defn- branch? [node]
  (satisfies? parent/Parent node))

(defn- get-children [node]
  (seq (parent/get-children node)))

(defn- new-node [node children]
  (parent/set-children node (vec children)))

(defn new-group-zipper [root]
  (z/zipper branch?
            get-children
            new-node
            root))

(defn- next-step [zipper]
  (let [current-node (z/node zipper)
        next (z/next zipper)]
    {:node current-node
     :zipper (if (not (z/end? next))
               next)}))

(defn traverse [zipper]
  (let [{zipper :zipper, node :node} (next-step zipper)]
    (if zipper
      (cons node (lazy-seq (traverse zipper)))
      (list node))))

(defn get-all-matching-objects [zipper predicate]
  (apply hash-set (filter predicate (traverse zipper))))

(defn get-all-non-group-objects [zipper]
  (get-all-matching-objects zipper
                            (complement
                             #(satisfies? parent/Parent %))))

(defn find-node
  ([zipper predicate]
   (let [next (z/next zipper)
         current-node (z/node next)]
     (if (predicate current-node)
       next     
       (if (z/end? next)
         nil
         (recur next predicate))))))

(defn path-to-node [zipper predicate]
  (z/path (find-node zipper predicate)))

(defn- go-to-parent [zipper]
  (let [new-zipper (z/up zipper)]
    (if new-zipper
      (recur new-zipper)
      zipper)))

(defn add-root-obj
  "Meaningful only in tests, I suspect

  Unwinds the object first"
  [zipper object]
  (z/append-child (go-to-parent zipper) object))

(defn update-zipper [k edit-f]
  (if-not (z/end? k)                               
    (recur (z/next (z/edit k edit-f)) edit-f)
    (z/root k)))


