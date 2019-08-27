(ns raytracer.transform
  (:require [raytracer.matrix :as matrix]))

(defn- translation [tx ty tz]
  (matrix/create [1 0 0 tx
                  0 1 0 ty
                  0 0 1 tz
                  0 0 0 1]))

(defn translate
  ([tx ty tz]
   (translation tx ty tz))
  ([tx ty tz m]
   (matrix/mul4 (translation tx ty tz) m)))

(defn- scaling [sx sy sz]
  (matrix/create [sx 0 0 0
                  0 sy 0 0
                  0 0 sz 0
                  0 0 0 1]))

(defn scale
  ([sx sy sz]
   (scaling sx sy sz))
  ([sx sy sz m]
   (matrix/mul4 (scaling sx sy sz) m)))

(defn- rotation-x [angle-rad]
  (let [cos (Math/cos angle-rad)
        sin (Math/sin angle-rad)]
    (matrix/create [1   0       0 0
                    0 cos (- sin) 0
                    0 sin     cos 0
                    0   0       0 1])))

(defn rotate-x
  ([angle-rad]
   (rotation-x angle-rad))
  ([angle-rad m]
   (matrix/mul4 (rotation-x angle-rad) m)))

(defn- rotation-y [angle-rad]
  (let [cos (Math/cos angle-rad)
        sin (Math/sin angle-rad)]
    (matrix/create [cos  0 sin 0
                    0  1   0 0
                    (- sin)  0 cos 0
                    0  0   0 1])))

(defn rotate-y
  ([angle-rad]
   (rotation-y angle-rad))
  ([angle-rad m]
   (matrix/mul4 (rotation-y angle-rad) m)))

(defn- rotation-z [angle-rad]
  (let [cos (Math/cos angle-rad)
        sin (Math/sin angle-rad)]
    (matrix/create [cos (- sin) 0 0
                    sin     cos 0 0
                    0       0 1 0
                    0       0 0 1])))

(defn rotate-z
  ([angle-rad]
   (rotation-z angle-rad))
  ([angle-rad m]
   (matrix/mul4 (rotation-z angle-rad) m)))

(defn- shearing
  [x-y x-z y-x y-z z-x z-y]
  (matrix/create [1 x-y x-z 0
                  y-x 1 y-z 0
                  z-x z-y 1 0
                  0     0 0 1]))

(defn shear
  ([x-y x-z y-x y-z z-x z-y]
   (shearing x-y x-z y-x y-z z-x z-y))
  ([x-y x-z y-x y-z z-x z-y m]
   (matrix/mul4 (shearing m x-y x-z y-x y-z z-x z-y) m)))
