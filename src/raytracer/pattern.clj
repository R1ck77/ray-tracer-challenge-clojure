(ns raytracer.pattern
  (:require [raytracer.matrix :as matrix]))

(defn stripe [white black]
  {:a white
   :b black
   :stripe-at (fn [pattern point]
                (if (zero? (mod (int (Math/floor (first point))) 2))
                  (:a pattern)
                  (:b pattern)))
   :transform matrix/identity-matrix
   :inverse-transform matrix/identity-matrix})

(defn change-transform [pattern new-transform]
  (merge pattern {:transform new-transform
                  :inverse-transform (matrix/invert new-transform 4)}))

(defn color-at-object [pattern object point]
  ((:stripe-at pattern) pattern (matrix/transform (:inverse-transform pattern)
                                                  (matrix/transform (:inverse-transform object)
                                                                    point))))
