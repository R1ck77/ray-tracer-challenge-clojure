(ns raytracer.pattern
  (:require [raytracer.color :as color]
            [raytracer.matrix :as matrix]))

(defn- create-pattern [white black function]
  {:a white
   :b black
   :color-at function
   :transform matrix/identity-matrix
   :inverse-transform matrix/identity-matrix})

(defn- stripe-function [pattern point]
  (if (zero? (mod (int (Math/floor (first point))) 2))
    (:a pattern)
    (:b pattern)))

(defn stripe [white black]
  (create-pattern white black stripe-function))

(defn- gradient-function [pattern point]
  (let [x (first point)
        p (- x (Math/floor x))]
    (color/add (color/scale (:a pattern) (- 1 x))
               (color/scale (:b pattern) x))))

(defn gradient [white black]
  (create-pattern white black gradient-function))

(defn- ring-function [pattern [x _ z _]]
  (let [distance (Math/sqrt (+ (* x x) (* z z)))]
                        (if (zero? (mod (int (Math/floor distance)) 2))
                          (:a pattern)
                          (:b pattern))))

(defn ring [white black]
  (create-pattern white black ring-function))

(defn- checker-function [pattern [x y z _]]
  (if (zero? (mod (int (+ (Math/floor x )
                          (Math/floor y)
                          (Math/floor z))) 2))
    (:a pattern)
    (:b pattern)))

(defn checker [white black]
  (create-pattern white black checker-function))

(defn change-transform [pattern new-transform]
  (merge pattern {:transform new-transform
                  :inverse-transform (matrix/invert new-transform 4)}))

(defn- point-in-pattern-space [pattern object point]
  (->> point
       (matrix/transform (:inverse-transform object))
       (matrix/transform (:inverse-transform pattern))))

(defn color-at-object [pattern object point]
  ((:color-at pattern) pattern (point-in-pattern-space pattern object point)))
