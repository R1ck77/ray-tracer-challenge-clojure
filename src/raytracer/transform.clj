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
