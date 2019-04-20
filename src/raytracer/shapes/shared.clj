(ns raytracer.shapes.shared
  (:require [raytracer.tuple :as tuple]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]
            [raytracer.intersection :as intersection]))

(defn as-point [[x y z _]]
  (point/point x y z))

(defn as-vector [[x y z _]]
  (svector/svector x y z))

(defn create-compute-normal-f [shape]
  (fn [point]
    (svector/normalize
     (as-vector
      (matrix/transform (matrix/transpose (:inverse-transform shape))
                        (tuple/sub (matrix/transform (:inverse-transform shape) point)
                                   (point/point 0 0 0)))))))

(defn add-normal-f [shape]
  (assoc shape :normal (create-compute-normal-f shape)))

