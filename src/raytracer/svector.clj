(ns raytracer.svector
  (:require [raytracer.tuple :as tuple]
            [raytracer.point :as point]))

(def ^:private max-error 1e-6)

(defn svector? [tuple]
  (< (Math/abs (double (- (:w tuple) 0)))
     max-error))

(defn svector [x y z]
  (tuple/tuple x y z 0))
