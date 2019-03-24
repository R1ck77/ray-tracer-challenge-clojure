(ns raytracer.point
  (:require [raytracer.tuples :refer :all]))

(defn point? [tuple]
  (eps= (nth tuple 3) 1))

(defn point [x y z]
  (vector x y z 1))

