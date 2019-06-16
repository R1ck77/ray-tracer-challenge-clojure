(ns raytracer.shapes.group
  (:require [raytracer.matrix :as matrix]))

(defrecord Group [shapes transform])

(defn group
  ([]
   (->Group [] matrix/identity-matrix)))
