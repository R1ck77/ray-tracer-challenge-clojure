;;; TODO/FIXME refactor this eldritch horror
(ns raytracer.shapes.cylinder
  (:require [raytracer.const :as const]
            [raytracer.tuple :as tuple]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]
            [raytracer.point :as point]
            [raytracer.material :as material]
            [raytracer.shapes.shared :as shared]
            [raytracer.intersection :as intersection]))

(defn is-within-bounds? [cylinder ray t]
  (let [y-intersection (+ (* (:y (:direction ray)) t) (:y (:origin ray)))]
    (and (> y-intersection (:minimum cylinder))
         (< y-intersection (:maximum cylinder)))))

(defn- create-intersections [cylinder ray t1 t2]
  (let [partial (if (is-within-bounds? cylinder ray t1)
                  [(intersection/intersection t1 cylinder)]
                  [])]
    (if (is-within-bounds? cylinder ray t2)
      (conj partial (intersection/intersection t2 cylinder))
      partial)))

(defn- local-intersect [cylinder ray-object-space]
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
            (create-intersections cylinder
                                  ray-object-space
                                  (- (/ (+ √discriminant b)
                                        (* 2 a)))
                                  (/ (- √discriminant  b)
                                     (* 2 a)))))))))

(defn- compute-cylinder-normal [point-object-space]
  (svector/svector (:x point-object-space) 0 (:z point-object-space)))

(defrecord Cylinder [minimum maximum closed inverse-transform])

(extend-type Cylinder
  shared/Intersectable
  (local-intersect [this ray-object-space]
    (local-intersect this ray-object-space))
  shared/Surface
  (compute-normal [this point]
    (tuple/normalize
     (shared/as-vector
      (matrix/transform (matrix/transpose (:inverse-transform this))
                        (compute-cylinder-normal (matrix/transform (:inverse-transform this) point)))))))

(defn cylinder 
  [& {:as args-map}]
  (let [args (merge {:transform matrix/identity-matrix
                     :minimum Double/NEGATIVE_INFINITY
                     :maximum Double/POSITIVE_INFINITY}
                    args-map)]
    (->Cylinder (:minimum args)
                (:maximum args)
                (:closed args)
                (matrix/invert (:transform args) 4))))
