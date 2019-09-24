(ns raytracer.shapes.smooth-triangle
  (:require [raytracer.const :as const]
            [raytracer.point :as point]
            [raytracer.tuple :as tuple]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]
            [raytracer.intersection :as intersection]
            [raytracer.material :as material]
            [raytracer.shapes.shared :as shared]
            [raytracer.shapes.bounding-box :as bounding-box]
            [raytracer.shapes.placement :as placement]
            [raytracer.grouping.shared :as gshared]))

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

(defn compute-interpolated-normal [triangle intersection]
  (let [[u v] (intersection/getUV intersection)]
    (tuple/add (tuple/mul (:n1 triangle) (- 1 u v))
               (tuple/add (tuple/mul (:n2 triangle) u)
                          (tuple/mul (:n3 triangle) v)))))

(defrecord SmoothTriangle [p1 p2 p3
                           n1 n2 n3
                           e1 e2
                           material placement
                           bounding-box]
  shared/Transformable
  (change-transform [this transform-matrix]
    (placement/change-shape-transform this transform-matrix))
  (get-placement [this] (:placement this))
  shared/Intersectable
  (local-intersect [this ray-in-sphere-space]
    (local-intersect-triangle this ray-in-sphere-space))
  (get-bounding-box [this]
    (bounding-box/transform (:bounding-box this)
                            (placement/get-transform (:placement this))))
  shared/Surface
  (compute-normal [this point hierarchy]
    (throw (UnsupportedOperationException. "Operation not supported")))
  (compute-normal [this _ intersection hierarchy]
    (gshared/local-to-world-coordinates hierarchy
                                        this
                                        (compute-interpolated-normal this intersection)))
  shared/Material
  (change-material [this new-material]
    (assoc this :material new-material))
  (get-material [this]
    (:material this))
  shared/Container
  (includes? [this object] (identical? this object)))

(defn- bounding-box-from-points [ & args]
  (apply bounding-box/create-box
         (bounding-box/extremes-from-points args)))

(defn smooth-triangle [p1 p2 p3 n1 n2 n3]
  (let [edge1 (tuple/sub p2 p1)
        edge2 (tuple/sub p3 p1)]
    (->SmoothTriangle p1 p2 p3
                      n1 n2 n3
                      edge1 edge2                
                      (material/material)
                      (placement/placement)
                      (bounding-box-from-points p1 p2 p3))))
