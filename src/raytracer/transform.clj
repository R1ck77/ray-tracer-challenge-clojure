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
    [1   0       0 0
     0 cos (- sin) 0
     0 sin     cos 0
     0   0       0 1]))

(defn rotation-y [angle-rad]
  (let [cos (Math/cos angle-rad)
        sin (Math/sin angle-rad)]
    [cos  0 sin 0
       0  1   0 0
 (- sin)  0 cos 0
       0  0   0 1]))

(defn rotation-z [angle-rad]
  (let [cos (Math/cos angle-rad)
        sin (Math/sin angle-rad)]
    [cos (- sin) 0 0
     sin     cos 0 0
       0       0 1 0
       0       0 0 1]))

(defn shearing
  [x-y x-z y-x y-z z-x z-y]
  [1 x-y x-z 0
   y-x 1 y-z 0
   z-x z-y 1 0
   0     0 0 1])
