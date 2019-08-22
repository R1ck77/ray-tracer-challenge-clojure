(ns raytracer.point
  (:require [raytracer.tuple :as tuple]))

(def ^:private max-error 1e-6)

(defn point? [tuple]
  (< (Math/abs (double (- (:w tuple) 1)))
     max-error))

(defn point [x y z]
  (tuple/tuple x y z 1.0))

(def origin (point 0.0 0.0 0.0))

