(ns raytracer.shapes.shared
  (:require [raytracer.point :as point]
            [raytracer.svector :as svector]))

(defn as-point [[x y z _]]
  (point/point x y z))

(defn as-vector [[x y z _]]
  (svector/svector x y z))
