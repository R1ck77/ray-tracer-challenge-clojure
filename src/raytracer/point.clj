(ns raytracer.point
  (:require [raytracer.tuple :refer :all]))

(def ^:private max-error 1e-6)

(defn point? [tuple]
  (< (Math/abs (- (nth tuple 3) 1))
     max-error))

(defn point [x y z]
  (vector x y z 1))

