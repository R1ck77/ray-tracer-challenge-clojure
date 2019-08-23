(ns raytracer.shapes.triangle
  (:require [raytracer.const :as const]
            [raytracer.point :as point]
            [raytracer.tuple :as tuple]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]
            [raytracer.intersection :as intersection]
            [raytracer.material :as material]
            [raytracer.shapes.shared :as shared]
            [raytracer.shapes.bounding-box :as bounding-box]
            [raytracer.shapes.placement :as placement]))

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
                [(intersection/uv-intersection (* f (tuple/dot e2 origin-cross-e1))
                                            triangle
                                            u v)]))))))))

(defrecord Triangle [p1 p2 p3 e1 e2 normal material placement]
  shared/Transformable
  (change-transform [this transform-matrix]
    (placement/change-shape-transform this transform-matrix))
  shared/Intersectable
  (local-intersect [this ray-in-sphere-space]
    (local-intersect-triangle this ray-in-sphere-space))
  shared/Surface
  (compute-normal [this point _]
    (shared/compute-normal this point))
  (compute-normal [this point]
    ;;; TODO/FIXME adding a change-transform would allow to optimize here a lot!
    (tuple/normalize
     (shared/as-vector
      (matrix/transform (-> this :placement placement/get-inverse-transposed-transform) normal))))
  bounding-box/BoundingBox
  (get-corners [this]
    (bounding-box/extremes-from-points [p1 p2 p3]))
  (hit [this ray] true)
  (get-transformed-points [this]
    (bounding-box/compute-filtered-transformed-extremes (bounding-box/get-corners this)
                                                        (-> this :placement placement/get-transform))))

(defn triangle [p1 p2 p3]
  (let [edge1 (tuple/sub p2 p1)
        edge2 (tuple/sub p3 p1)]
    (->Triangle p1 p2 p3
                edge1 edge2
                (tuple/normalize (tuple/cross edge2 edge1))
                (material/material)
                (placement/placement))))
