(ns raytracer.ray
  (:require [raytracer.utils :as utils]
            [raytracer.tuple :as tuple]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]))

(set! *unchecked-math* true)

(defrecord Ray [origin direction])

;;; TODO/FIXME use ray/ray instead (as for other objects)
(defn create [origin direction]
  (->Ray origin direction))

(defn position [ray t]
  (tuple/add (:origin ray)
             (svector/mul (:direction ray) t)))

(defn sphere []
  {:center [0 0 0 1]
   :radius 1.0
   :transform matrix/identity-matrix})

(defn change-transform [sphere new-transform]
  (merge sphere {:transform new-transform
                 :inverse-transform (matrix/invert new-transform 4)}))

(defn ray-sphere-discriminant [ray sphere]
  (let [sphere-to-ray (tuple/sub (:origin ray)
                                 (:center sphere))
        a (svector/dot (:direction ray)
                       (:direction ray))
        b (* 2 (svector/dot (:direction ray)
                            sphere-to-ray))
        c (- (svector/dot sphere-to-ray sphere-to-ray) 1)]
    [a b c (- (* b b ) (* 4 a c))]))

(defrecord Intersection [t object])

(defn intersection [t object]
  (->Intersection t object))

(defn transform [ray matrix]
  (create (matrix/transform matrix (:origin ray))
          (matrix/transform matrix (:direction ray))))

(defn intersect-sphere-space [ray-in-sphere-space sphere]
  (let [[a b c discriminant]
        (ray-sphere-discriminant ray-in-sphere-space sphere)]
    (if (< discriminant 0)
      {:count 0   ;;; TODO/FIXME :count is bad for clarity. Use :n instead or :n-int, or something else
       :values []}
      {:count 2
       :values [(intersection (/ (- (- b) (Math/sqrt discriminant))
                                 (* 2 a))
                              sphere)
                (intersection (/ (+ (- b) (Math/sqrt discriminant))
                                 (* 2 a))
                              sphere)]})))

(defn intersect [ray sphere]
  (intersect-sphere-space (transform ray (:inverse-transform sphere))
                          sphere))

(defn intersections [ & args]
  (vec args))

(defn- non-backward? [intersection]
  (>= (:t intersection) 0))

(defn hit [xinters]
  (first (sort-by #(:t %) (filter non-backward? xinters))))
