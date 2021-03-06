(ns raytracer.grouping.zipper
  (:require [clojure.zip :as z]
            [raytracer.matrix :as matrix]
            [raytracer.grouping.shared :as shared]
            [raytracer.shapes.group :as group]
            [raytracer.shapes.placement :as placement])
  (:import [raytracer.shapes.group Parent Group EmptyGroup]))

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

(defn- get-all-non-group-objects [zipper]
  (get-all-matching-objects zipper
                            (complement
                             #(satisfies? group/Parent %))))

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
           (map #(-> % :placement placement/get-inverse-transposed-transform)
                (conj (z/path (do-find-node zipper #(= shape %)))
                      shape)))))

(defn- do-compute-world-to-local-transform  [zipper shape]
  (reduce #(matrix/mul4 %2 %)
          (map #(-> % :placement placement/get-inverse-transform)
               (conj (z/path (do-find-node zipper #(= shape %)))
                     shape))))

(defn- branch? [node]
  (satisfies? group/Parent node))

(defn- get-children [node]
  (seq (group/get-children node)))

(defn- new-node [node children]
  (group/set-children node (vec children)))

(defprotocol GroupZipper
  (compute-local-to-world-transform [this shape])
  (compute-world-to-local-transform [this shape])
  (find-node [this predicate]))

(defn- new-group-zipper [root]
  (z/zipper branch?
            get-children
            new-node
            root))

(defn- go-to-parent [zipper]
  (let [new-zipper (z/up zipper)]
    (if new-zipper
      (recur new-zipper)
      zipper)))

(defn- add-root-obj
  "Meaningful only in tests, I suspect

  Unwinds the object first"
  [zipper object]
  (z/append-child (go-to-parent zipper) object))

(defn- edit-zipper [k edit-f]
  (if-not (z/end? k)                               
    (recur (z/next (z/edit k edit-f)) edit-f)
    (z/root k)))

(defn create-zipper
  ([root]
   (let [zipper (new-group-zipper root)]
     (create-zipper root zipper)))
  ([root zipper]
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
       (get-all-non-group-objects zipper))
     shared/HierarchyEditor
     (add-root-object [this object]
       (create-zipper root (add-root-obj zipper object)))
     (update-objects [this update-f]
       (create-zipper (edit-zipper zipper update-f))))))
