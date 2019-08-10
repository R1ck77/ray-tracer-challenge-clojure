(ns raytracer.shapes.triangle
  (:require [raytracer.const :as const]
            [raytracer.point :as point]
            [raytracer.tuple :as tuple]
            [raytracer.svector :as svector]
            [raytracer.intersection :as intersection]
            [raytracer.shapes.shared :as shared]))

(defn- local-intersect-triangle [triangle ray]
  (let [dir-cross-e2 (tuple/cross (:direction ray) (:e2 triangle))
        det (tuple/dot (:e1 triangle) dir-cross-e2)]
    (if (< (Math/abs (float det)) const/EPSILON)
      []
      (let [f (/ det)
            p1-to-origin (tuple/sub (:origin ray) (:p1 triangle))
            u (* f (tuple/dot p1-to-origin dir-cross-e2))]
        (cond)
        (if (or (< u 0) (> u 1))
          []
          (let [origin-cross-e1 (tuple/cross p1-to-origin (:e1 triangle))
                v (* f (tuple/dot (:direction ray) origin-cross-e1))]
            (if (or (< v 0) (> (+ v u) 1))
              []
              [(intersection/intersection (* f (tuple/dot (:e2 triangle)
                                                          origin-cross-e1))
                                          triangle)])))))))

(defrecord Triangle [p1 p2 p3 e1 e2 normal]
  shared/Surface
  (compute-normal [this point]
    normal)
  shared/Intersectable
  (local-intersect [this ray]
    (local-intersect-triangle this ray)))

(defn triangle [p1 p2 p3]
  (let [edge1 (tuple/sub p2 p1)
        edge2 (tuple/sub p3 p1)]
    (->Triangle p1 p2 p3
                edge1 edge2
                (tuple/normalize (tuple/cross edge2 edge1)))))