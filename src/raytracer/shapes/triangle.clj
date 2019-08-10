(ns raytracer.shapes.triangle
  (:require [raytracer.point :as point]
            [raytracer.tuple :as tuple]
            [raytracer.svector :as svector]
            [raytracer.shapes.shared :as shared]))

(defrecord Triangle [p1 p2 p3 e1 e2 normal]
  shared/Surface
  (compute-normal [this point]
    normal)
  shared/Intersectable
  (local-intersect [this ray]
    (throw (UnsupportedOperationException.))))

(defn triangle [p1 p2 p3]
  (let [edge1 (tuple/sub p2 p1)
        edge2 (tuple/sub p3 p1)]
    (->Triangle p1 p2 p3
                edge1 edge2
                (tuple/normalize (tuple/cross edge2 edge1)))))
