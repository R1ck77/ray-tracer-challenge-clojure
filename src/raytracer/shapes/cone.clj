(ns raytracer.shapes.cone
  (:require [raytracer.utils :as utils]
            [raytracer.const :as const]
            [raytracer.tuple :as tuple]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]
            [raytracer.point :as point]
            [raytracer.material :as material]
            [raytracer.shapes.shared :as shared]
            [raytracer.intersection :as intersection]))

(defn- check-cap [ray t]
  (let [point (tuple/add (:origin ray) (tuple/mul (:direction ray) t))
        x (:x point)
        z (:z point)]
    (<= (+ (* x x)
           (* z z))
        1)))

(defn- intersect-cap [cone ray]
  (let [y-direction (-> ray :direction :y)]
    (if (or (not (:closed cone))
            (< (-> y-direction float Math/abs) const/EPSILON))
      []
      ;;; TODO/FIXME? slow, probably
      (map #(intersection/intersection % cone)
           (filter #(check-cap ray %)
                   (map #(/ (- (get cone %)
                               (-> ray :origin :y))
                            y-direction)  [:minimum :maximum]))))))

(defn is-within-bounds? [cone ray t]
  (and t
      (let [y-intersection (+ (* (:y (:direction ray)) t) (:y (:origin ray)))]
        (and (> y-intersection (:minimum cone))
             (< y-intersection (:maximum cone))))))

(defn- create-intersections [cone ray [t1 t2 :as xt]]
  (if xt
    (utils/map-filter #(intersection/intersection % cone)
                      #(is-within-bounds? cone ray %)
                      [t1 t2])))

(defmacro map-to-vector [f & args]
  `(vector ~@(map #(list f %) args)))

(defn- t-for-side-intersections [ray-object-space]
  (let [direction (:direction ray-object-space)
        x-direction (:x direction)
        y-direction (:y direction)
        z-direction (:z direction)
        a (+ (* x-direction x-direction)
             (- (* y-direction y-direction))
             (* z-direction z-direction))
        origin (:origin ray-object-space)
        x-origin (:x origin)
        y-origin (:y origin)
        z-origin (:z origin)
        b (+ (* 2 x-origin x-direction)
             (- (* 2 y-origin y-direction))
             (* 2 z-origin z-direction))
        c (+ (* x-origin x-origin)
             (- (* y-origin y-origin))                 
             (* z-origin z-origin))]
    (if (>= a const/EPSILON)
      (let [discriminant (- (* b b)
                            (* 4 a c))]
        (if (>= discriminant 0)
          (let [√discriminant (Math/sqrt discriminant)]
            (list (- (/ (+ √discriminant b)
                        (* 2 a)))
                  (/ (- √discriminant  b)
                     (* 2 a))))))
      (if (> (Math/abs (float b)) const/EPSILON)
        [(- (/ c (* 2 b)))]))))

(defn- intersect-sides [cone ray-object-space]
  (create-intersections cone
                        ray-object-space
                        (t-for-side-intersections ray-object-space)))

(defn- local-intersect [cone ray-object-space]
  (concat (intersect-sides cone ray-object-space)
          (intersect-cap cone ray-object-space)))

(defn- compute-cone-side-normal [point-object-space]
  (let [px (:x point-object-space)
        py (:y point-object-space)
        pz (:z point-object-space)
        y (Math/sqrt (+ (* px px)
                        (* pz pz)))]
    (tuple/normalize
     (svector/svector px (if (> py 0)
                           (- y)
                           y)
                      pz))))

(defn- compute-cone-normal [this point-object-space]
  (let [x (:x point-object-space)
        y (:y point-object-space)
        z (:z point-object-space)
        dist (+ (* x x) (* z z))]
    (cond 
      (and (< dist 1) (>= y (- (:maximum this) const/EPSILON))) (svector/svector 0 1 0)
      (and (< dist 1) (<= y (+ (:minimum this) const/EPSILON))) (svector/svector 0 -1 0)
      :default (compute-cone-side-normal point-object-space))))

(defrecord Cone [minimum maximum closed inverse-transform])

(extend-type Cone
  shared/Intersectable
  (local-intersect [this ray-object-space]
    (local-intersect this ray-object-space))
  shared/Surface
  (compute-normal [this point]
    (tuple/normalize
     (shared/as-vector
      (matrix/transform (matrix/transpose (:inverse-transform this))
                        (compute-cone-normal this
                                             (matrix/transform (:inverse-transform this) point)))))))

(defn cone 
  [& {:as args-map}]
  (let [args (merge {:transform matrix/identity-matrix
                     :minimum Double/NEGATIVE_INFINITY
                     :maximum Double/POSITIVE_INFINITY}
                    args-map)]
    (->Cone (:minimum args)
            (:maximum args)
            (:closed args)
            (matrix/invert (:transform args) 4))))
