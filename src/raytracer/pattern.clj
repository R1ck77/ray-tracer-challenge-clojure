(ns raytracer.pattern
  (:require [raytracer.color :as color]
            [raytracer.matrix :as matrix]))

(defn- create-pattern [white black function]
  {:a white
   :b black
   :color-at function
   :transform matrix/identity-matrix
   :inverse-transform matrix/identity-matrix})

(defn- solid-function [pattern _]
  (:a pattern))

(defn solid [color]
  (create-pattern color color solid-function))

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

(defn- test-pattern-function [pattern [x y z _]]
  [x y z])

(defn test []
  (create-pattern nil nil test-pattern-function))

(defn- ring-function [pattern [x _ z _]]
  (let [distance (Math/sqrt (+ (* x x) (* z z)))]
                        (if (zero? (mod (int (Math/floor distance)) 2))
                          (:a pattern)
                          (:b pattern))))

(defn ring [white black]
  (create-pattern white black ring-function))

(defn- correct-sign [x]
  (if (< x 0)
    (inc (- x))
    x))

(defn- checker-function [pattern [x y z _]]
  (if (zero? (mod (+ (int (correct-sign x))
                     (int (correct-sign y))
                     (int (correct-sign z))) 2))
    (:a pattern)
    (:b pattern)))

(defn checker [white black]
  (create-pattern white black checker-function))

(defn blend-function [pattern point]
  (let [a (:a-pattern pattern)
        b (:b-pattern pattern)]
    (color/scale (color/add ((:color-at a) a point)
                            ((:color-at b) b point))
                 0.5)))

(defn blend [_ pattern-a pattern-b]
  {:transform matrix/identity-matrix
   :inverse-transform matrix/identity-matrix
   :a-pattern pattern-a
   :b-pattern pattern-b
   :color-at blend-function})

(defn perturb-pattern [pattern perturbation-f]
  (update pattern :color-at (fn [blend-function]
                              (fn [pattern point]
                                (blend-function pattern (perturbation-f point))))))

(defn change-transform [pattern new-transform]
  (merge pattern {:transform new-transform
                  :inverse-transform (matrix/invert new-transform 4)}))

(defn- point-in-pattern-space [pattern object point]
  (->> point
       (matrix/transform (:inverse-transform object))
       (matrix/transform (:inverse-transform pattern))))

(defn color-at-object [pattern object point]
  ((:color-at pattern) pattern (point-in-pattern-space pattern object point)))
