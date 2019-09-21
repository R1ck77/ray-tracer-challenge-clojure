(ns raytracer.shapes.sphere
  (:require [raytracer.tuple :as tuple]
            [raytracer.svector :as svector]
            [raytracer.point :as point]
            [raytracer.matrix :as matrix]
            [raytracer.shapes.shared :as shared]
            [raytracer.material :as material]
            [raytracer.intersection :as intersection]
            [raytracer.shapes.bounding-box :as bounding-box]
            [raytracer.shapes.placement :as placement]))

(def center (point/point 0 0 0))

(def sphere-bounding-box (bounding-box/create-box (point/point -1 -1 -1)
                                                  (point/point 1 1 1)))

(defrecord Sphere [material placement bounding-box])

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

(defn compute-normal [shape point hierarchy]
  (shared/decorated-compute-normal (fn [_ point]
                                     (tuple/sub point (point/point 0 0 0)))
                                   shape
                                   point
                                   hierarchy))

(extend-type Sphere
  shared/Transformable
  (change-transform [this transform-matrix]
    (placement/change-shape-transform this transform-matrix))
  (get-placement [this] (:placement this))
  shared/Intersectable
  (local-intersect [this ray-in-sphere-space]
    (intersect-sphere-space this ray-in-sphere-space))
  (get-bounding-box [this]
    (:bounding-box this))
  shared/Surface
  (compute-normal
    ([this point _ hierarchy]
     (shared/compute-normal this point hierarchy))
   ([this point hierarchy]
    (compute-normal this point hierarchy)))
  shared/Material
  (change-material [this new-material]
    (assoc this :material new-material))
  (get-material [this]
    (:material this))
  shared/Container
  (includes? [this object] (identical? this object)))

(defn sphere [& {:as args-map}]
  (let [transform (or (:transform args-map)
                      matrix/identity-matrix)
        bounding-box (bounding-box/transform sphere-bounding-box transform)]
    (->Sphere (or (:material args-map) material/default-material)
              (placement/placement transform)
              bounding-box)))
