(ns raytracer.colors
  (:require [raytracer.tuples :as tuples :refer [is-about?]]))

(defn eq-color? [[r1 g1 b1] [r2 g2 b2]]
  (and (is-about? r1 r2)
       (is-about? g1 g2)
       (is-about? b1 b2)))

(defn add [a b]
  (tuples/add a b))

(defn sub [a b]
  (tuples/sub a b))

(defn mul [[r1 g1 b1] [r2 g2 b2]]
  (vector (* r1 r2)
          (* g1 g2)
          (* b1 b2)))

(defn scale [[r g b] s]
  (vector (* r s)
          (* g s)
          (* b s)))


