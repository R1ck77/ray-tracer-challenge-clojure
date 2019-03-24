(ns raytracer.svector
  (:require [raytracer.tuples :as tuples :refer [eps=]]))

(defn svector? [tuple]
  (eps= (nth tuple 3) 0))

(defn svector [x y z]
  (vector x y z 0))

(defn add [a b]
  (tuples/add a b))

(defn sub [a b]
  (tuples/sub a b))

(defn mul [[x y z w] s]
  (vector (* x s)
          (* y s)
          (* z s)
          (* w s)))

(defn div [[x y z w] s]
  (vector (/ x s)
          (/ y s)
          (/ z s)
          (/ w s)))

(defn mag [[x y z w]]
  (Math/sqrt (float (+ (* x x)
                       (* y y)
                       (* z z)
                       (* w w)))))

(defn norm [[x y z w :as v]]
  (let [m (mag v)]
    (vector (/ x m)
            (/ y m)
            (/ z m)
            (/ w m))))

(defn dot [[x1 y1 z1 w1] [x2 y2 z2 w2]]
  (+ (* x1 x2)
     (* y1 y2)
     (* z1 z2)
     (* w1 w2)))

(defn cross [[x1 y1 z1 _] [x2 y2 z2 _]]
  (svector (- (* y1 z2) (* z1 y2))
           (- (* z1 x2) (* x1 z2))
           (- (* x1 y2) (* y1 x2))))

