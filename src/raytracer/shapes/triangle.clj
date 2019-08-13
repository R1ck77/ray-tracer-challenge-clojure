(ns raytracer.shapes.triangle
  (:require [raytracer.const :as const]
            [raytracer.point :as point]
            [raytracer.tuple :as tuple]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]
            [raytracer.intersection :as intersection]
            [raytracer.material :as material]
            [raytracer.shapes.shared :as shared]
            [raytracer.shapes.bounding-box :as bounding-box]))

(defn- local-intersect-triangle [{:keys [p1 e1 e2] :as triangle} {:keys [origin direction] :as ray}]
  (let [dir-cross-e2 (tuple/cross direction e2)
        det (tuple/dot e1 dir-cross-e2)]
    (if (< (Math/abs (float det)) const/EPSILON)
      []
      (let [f (/ det)]
        (let [p1-to-origin (tuple/sub origin p1)
              u (* f (tuple/dot p1-to-origin dir-cross-e2))]
          (if (or (< u 0) (> u 1))
            []
            (let [origin-cross-e1 (tuple/cross p1-to-origin e1)
                  v (* f (tuple/dot direction origin-cross-e1))]
              (if (or (< v 0) (> (+ v u) 1))
                []
                [(intersection/intersection (* f (tuple/dot e2 origin-cross-e1))
                                            triangle)]))))))))

(defrecord Triangle [p1 p2 p3 e1 e2 normal material transform inverse-transform inverse-transposed-transform]
  shared/Transformable
  (transform [this transform-matrix]
    (shared/change-transform this transform-matrix))
  shared/Intersectable
  (local-intersect [this ray-in-sphere-space]
    (local-intersect-triangle this ray-in-sphere-space))
  shared/Surface
  (compute-normal [this point]
    normal)
  bounding-box/BoundingBox
  (get-corners [this]
    (bounding-box/extremes-from-points [p1 p2 p3]))
  (hit [this ray] true)
  (get-transformed-points [this]
    (bounding-box/compute-filtered-transformed-extremes (bounding-box/get-corners this)
                                                        (:transform this))))

(defn triangle [p1 p2 p3]
  (let [edge1 (tuple/sub p2 p1)
        edge2 (tuple/sub p3 p1)]
    (->Triangle p1 p2 p3
                edge1 edge2
                (tuple/normalize (tuple/cross edge2 edge1))
                (material/material)
                matrix/identity-matrix
                matrix/identity-matrix
                matrix/identity-matrix)))
