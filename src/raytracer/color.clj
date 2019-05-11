(ns raytracer.color
  (:require [raytracer.tuple :as tuple]))

(def black [0 0 0])

(defn add [a b]
  (vector (+ (nth a 0) (nth b 0))
          (+ (nth a 1) (nth b 1))
          (+ (nth a 2) (nth b 2))))

(defn sub [a b]
  (vector (- (nth a 0) (nth b 0))
          (- (nth a 1) (nth b 1))
          (- (nth a 2) (nth b 2))))

(defn mul [[r1 g1 b1] [r2 g2 b2]]
  (vector (* r1 r2)
          (* g1 g2)
          (* b1 b2)))

(defn scale [[r g b] s]
  (vector (* r s)
          (* g s)
          (* b s)))


