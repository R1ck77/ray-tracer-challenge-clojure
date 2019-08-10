(ns raytracer.shapes.triangle
  (:require [raytracer.point :as point]
            [raytracer.tuple :as tuple]
            [raytracer.svector :as svector]))

(defrecord Triangle [p1 p2 p3 e1 e2 normal])

(defn triangle [p1 p2 p3]
  (let [edge1 (tuple/sub p2 p1)
        edge2 (tuple/sub p3 p1)]
    (->Triangle p1 p2 p3
                edge1 edge2
                (tuple/normalize (tuple/cross edge2 edge1)))))
