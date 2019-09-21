(ns raytracer.shapes.cylinder
  (:require [raytracer.utils :as utils]
            [raytracer.const :as const]
            [raytracer.tuple :as tuple]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]
            [raytracer.point :as point]
            [raytracer.material :as material]
            [raytracer.shapes.shared :as shared]
            [raytracer.intersection :as intersection]
            [raytracer.shapes.bounding-box :as bounding-box]
            [raytracer.shapes.placement :as placement]))

(defn- check-cap [ray t]
  (let [point (tuple/add (:origin ray) (tuple/mul (:direction ray) t))
        x (:x point)
        z (:z point)]
    (<= (+ (* x x)
           (* z z))
        1)))

(defn- parallel? [y-direction]
  (< (-> y-direction float Math/abs)
     const/EPSILON))

(defn- intermediate-value [y-direction y-origin cap-y]
  (/ (- cap-y y-origin)
     y-direction))

(defn- intersect-cap [cylinder ray]
  (let [y-direction (-> ray :direction :y)
        y-origin (-> ray :origin :y)
        intermediate-value (partial intermediate-value y-direction y-origin)]
    (if (or (not (:closed cylinder))
            (parallel? y-direction))
      []
      (utils/map-filter #(intersection/intersection % cylinder)
                        #(check-cap ray %)
                        [(intermediate-value (get cylinder :minimum))
                         (intermediate-value (get cylinder :maximum))]))))

(defn is-within-bounds? [cylinder ray t]
  (let [y-intersection (+ (* (:y (:direction ray)) t) (:y (:origin ray)))]
    (and (> y-intersection (:minimum cylinder))
         (< y-intersection (:maximum cylinder)))))

(defn- create-intersections [cylinder ray [t1 t2 :as xt]]
  (if xt
    (utils/map-filter #(intersection/intersection % cylinder)
                      #(is-within-bounds? cylinder ray %)
                      [t1 t2])))

(defmacro map-to-vector [f & args]
  `(vector ~@(map #(list f %) args)))

(defn- t-for-side-intersections [ray-object-space]
  (let [direction (:direction ray-object-space)
        x-direction (:x direction)
        z-direction (:z direction)
        a (+ (* x-direction x-direction)
             (* z-direction z-direction))]
    (if (>= a const/EPSILON)
      (let [origin (:origin ray-object-space)
            x-origin (:x origin)
            z-origin (:z origin)
            b (+ (* 2 x-origin x-direction)
                 (* 2 z-origin z-direction))
            c (+ (* x-origin x-origin)
                 (* z-origin z-origin)
                 -1)
            discriminant (- (* b b)
                            (* 4 a c))]
        (if (>= discriminant 0)
          (let [√discriminant (Math/sqrt discriminant)]
            (list (- (/ (+ √discriminant b)
                        (* 2 a)))
                  (/ (- √discriminant  b)
                     (* 2 a)))))))))

(defn- intersect-sides [cylinder ray-object-space]
  (create-intersections cylinder
                        ray-object-space
                        (t-for-side-intersections ray-object-space)))

(defn- local-intersect [cylinder ray-object-space]
  (concat (intersect-sides cylinder ray-object-space)
          (intersect-cap cylinder ray-object-space)))

(defn- compute-cylinder-side-normal [point-object-space]
    (svector/svector (:x point-object-space) 0 (:z point-object-space)))

(defn- compute-cylinder-normal [this point-object-space]
  (let [x (:x point-object-space)
        y (:y point-object-space)
        z (:z point-object-space)
        dist (+ (* x x) (* z z))]
    (cond 
      (and (< dist 1) (>= y (- (:maximum this) const/EPSILON))) (svector/svector 0 1 0)
      (and (< dist 1) (<= y (+ (:minimum this) const/EPSILON))) (svector/svector 0 -1 0)
      :default (compute-cylinder-side-normal point-object-space))))

(defrecord Cylinder [material minimum maximum closed placement bounding-box])

(extend-type Cylinder
  shared/Transformable
  (change-transform [this transform-matrix]
    (placement/change-shape-transform this transform-matrix))
  (get-placement [this] (:placement this))
  shared/Intersectable
  (local-intersect [this ray-object-space]
    (local-intersect this ray-object-space))
  (get-bounding-box [this]
    (:bounding-box this))
  shared/Surface
  (compute-normal
    ([this point _ hierarchy]
     (shared/compute-normal this point hierarchy))
    ([this point hierarchy]
     (shared/decorated-compute-normal compute-cylinder-normal
                                      this
                                      point
                                      hierarchy)))
  shared/Material
  (change-material [this new-material]
    (assoc this :material new-material))
  (get-material [this]
    (:material this))
  shared/Container
  (includes? [this object] (identical? this object)))

(defn cylinder 
  [& {:as args-map}]
  (let [args (merge {:transform matrix/identity-matrix
                     :minimum const/neg-inf
                     :maximum const/inf
                     :material material/default-material}
                    args-map)
        min (:minimum args)
        max (:maximum args)]
    (->Cylinder (:material args)
                (:minimum args)
                (:maximum args)
                (:closed args)
                (-> args :transform placement/placement)
                (bounding-box/create-box (point/point -1 min -1)
                                         (point/point 1 max 1)))))
