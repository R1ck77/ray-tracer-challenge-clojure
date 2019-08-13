(ns raytracer.shapes.sphere
  (:require [raytracer.tuple :as tuple]
            [raytracer.svector :as svector]
            [raytracer.point :as point]
            [raytracer.matrix :as matrix]
            [raytracer.shapes.shared :as shared]
            [raytracer.material :as material]
            [raytracer.intersection :as intersection]
            [raytracer.shapes.bounding-box :as bounding-box]))

(def center (point/point 0 0 0))

(defrecord Sphere [material transform inverse-transform inverse-transposed-transform])

(defn- ray-sphere-discriminant [this-sphere ray]
  (let [sphere-to-ray (tuple/sub (:origin ray)
                                 center)
        a (tuple/dot (:direction ray)
                     (:direction ray))
        b (* 2 (tuple/dot (:direction ray)
                          sphere-to-ray))
        c (- (tuple/dot sphere-to-ray sphere-to-ray) 1)]
    [a b c (- (* b b ) (* 4 a c))]))

(defn- intersect-sphere-space [this-sphere ray-in-sphere-space]
  (let [[a b c discriminant]
        (ray-sphere-discriminant this-sphere ray-in-sphere-space)]
    (if (< discriminant 0)
      []
      [(intersection/intersection (/ (- (- b) (Math/sqrt discriminant))
                                     (* 2 a))
                                  this-sphere)
       (intersection/intersection (/ (+ (- b) (Math/sqrt discriminant))
                                     (* 2 a))
                                  this-sphere)])))

(defn compute-normal [shape point]
  (tuple/normalize
   (shared/as-vector
    (matrix/transform (:inverse-transposed-transform shape)
                      (tuple/sub (matrix/transform (:inverse-transform shape) point)
                                 (point/point 0 0 0))))))

(extend-type Sphere
  shared/Transformable
  (transform [this transform-matrix]
    (shared/change-transform this transform-matrix))
  shared/Intersectable
  (local-intersect [this ray-in-sphere-space]
    (intersect-sphere-space this ray-in-sphere-space))
  shared/Surface
  (compute-normal [this point]
    (compute-normal this point))
  bounding-box/BoundingBox
  (get-corners [this]
    (vector (point/point -1 -1 -1)
            (point/point 1 1 1)))
  (hit [this ray] true)
  (get-transformed-points [this]
    (bounding-box/compute-filtered-transformed-extremes (bounding-box/get-corners this)
                                                        (:transform this))))
;;; TODO/FIXME This should go in a transform cache
(defn- quick-invert [m]
  (if (= m matrix/identity-matrix)
    matrix/identity-matrix
    (matrix/invert m 4)))

(defn- quick-transpose [m]
  (if (= m matrix/identity-matrix)
    matrix/identity-matrix
    (matrix/transpose m)))

(defn sphere [& {:as args-map}]
  (let [transform (or (:transform args-map)
                      matrix/identity-matrix)
        inverse (or (:inverse-transform args-map) (quick-invert transform))
        inverse-transposed (quick-transpose inverse)]
    (map->Sphere 
     {:material (or (:material args-map) (material/material))
      :transform transform
      :inverse-transform inverse
      :inverse-transposed-transform inverse-transposed})))
