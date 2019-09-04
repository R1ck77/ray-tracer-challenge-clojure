(ns raytracer.shapes.csg
  (:require [raytracer.intersection :as intersection]
            [raytracer.shapes.shared :as shared]))

(defprotocol CSG
  (is-intersection-allowed? [this left-shape-hit inside-left-shape inside-right-shape]))

(defrecord CSGUnion []
  CSG
  (is-intersection-allowed? [this left-shape-hit inside-left-shape inside-right-shape]
    (or (and left-shape-hit (not inside-right-shape))
        (and (not left-shape-hit) (not inside-left-shape)))))

(defn union []
  (->CSGUnion))

(defrecord CSGIntersection []
  CSG
  (is-intersection-allowed? [this left-shape-hit inside-left-shape inside-right-shape]
    (or (and left-shape-hit inside-right-shape)
        (and (not left-shape-hit) inside-left-shape))))

(defn intersection []
  (->CSGIntersection))

(defrecord CSGDifference []
  CSG
  (is-intersection-allowed? [this left-shape-hit inside-left-shape inside-right-shape]
    (or (and left-shape-hit (not inside-right-shape))
        (and (not left-shape-hit) inside-left-shape))))

(defn difference []
  (->CSGDifference))

