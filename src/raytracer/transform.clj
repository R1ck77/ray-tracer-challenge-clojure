(ns raytracer.transform
  (:require [raytracer.matrix :as matrix]))

(defn translation [x y z]
  [1 0 0 x
   0 1 0 y
   0 0 1 z
   0 0 0 1])

(defn scaling [sx sy sz]
  [sx 0 0 0
   0 sy 0 0
   0 0 sz 0
   0 0 0 1])

(defn rotation-x [angle-rad]
  (let [cos (Math/cos angle-rad)
        sin (Math/sin angle-rad)]
    [1 0 0 0
     0 cos (- sin) 0
     0 sin cos 0
     0 0 0 1]))
