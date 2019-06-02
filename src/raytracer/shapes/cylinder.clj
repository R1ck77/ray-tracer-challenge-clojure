(ns raytracer.shapes.cylinder
  (:require [raytracer.const :as const]
            [raytracer.tuple :as tuple]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]
            [raytracer.point :as point]
            [raytracer.material :as material]
            [raytracer.shapes.shared :as shared]
            [raytracer.intersection :as intersection]))


(defn- local-intersect [this ray-object-space]
  (let [direction (:direction ray-object-space)
        a (+ (* (:x direction) (:x direction))
             (* (:z direction) (:z direction)))]
    (if (< a const/EPSILON)
      []
      (let [origin (:origin ray-object-space)
            b (+ (* 2 (:x origin) (:x direction))
                 (* 2 (:z origin) (:z direction)))
            c (+ (* (:x origin) (:x origin))
                 (* (:z origin) (:z origin))
                 -1)
            discriminant (- (* b b) (* 4 a c))]
        (if (< discriminant 0)
          []
          (let [√discriminant (Math/sqrt discriminant)]
            [(intersection/intersection (- (/ (+ √discriminant b)
                                              (* 2 a)))
                                        cylinder)
             (intersection/intersection (/ (- √discriminant  b)
                                           (* 2 a))
                                        cylinder)]))))))



(defn cylinder []
  (reify shared/Intersectable
    (local-intersect [this ray-object-space]
      (local-intersect this ray-object-space))))
