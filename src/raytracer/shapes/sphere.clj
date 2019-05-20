(ns raytracer.shapes.sphere
  (:require [raytracer.tuple :as tuple]
            [raytracer.svector :as svector]
            [raytracer.point :as point]
            [raytracer.matrix :as matrix]
            [raytracer.shapes.shared :as shared]
            [raytracer.material :as material]
            [raytracer.intersection :as intersection]))

(def center (point/point 0 0 0))

(defrecord Sphere [material transform inverse-transform])

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
    (matrix/transform (matrix/transpose (:inverse-transform shape))
                      (tuple/sub (matrix/transform (:inverse-transform shape) point)
                                 (point/point 0 0 0))))))

(extend-type Sphere
  shared/Intersectable
  (local-intersect [this ray-in-plane-space]
    (intersect-sphere-space this ray-in-plane-space))
  shared/Surface
  (compute-normal [this point]
    (compute-normal this point)))

(defn sphere []
  (map->Sphere 
   {:material (material/material)
    :transform matrix/identity-matrix
    :inverse-transform matrix/identity-matrix}))
