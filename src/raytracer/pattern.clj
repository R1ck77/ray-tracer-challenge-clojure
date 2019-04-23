(ns raytracer.pattern
  (:require [raytracer.color :as color]
            [raytracer.matrix :as matrix]))

(def base-pattern {:transform matrix/identity-matrix
                   :inverse-transform matrix/identity-matrix})

(defn stripe [white black]
  (merge {:a white
    :b black
    :color-at (fn [pattern point]
                (if (zero? (mod (int (Math/floor (first point))) 2))
                  (:a pattern)
                  (:b pattern)))}
         base-pattern))

(defn gradient [from to]
  (merge {:a from
          :b to
          :color-at (fn [pattern point]
                      (let [x (first point)
                            p (- x (Math/floor x))]
                        (color/add (color/scale from (- 1 x))
                                   (color/scale to x))))}
         base-pattern))

(defn change-transform [pattern new-transform]
  (merge pattern {:transform new-transform
                  :inverse-transform (matrix/invert new-transform 4)}))

(defn- point-in-pattern-space [pattern object point]
  (->> point
       (matrix/transform (:inverse-transform object))
       (matrix/transform (:inverse-transform pattern))))

(defn color-at-object [pattern object point]
  ((:color-at pattern) pattern (point-in-pattern-space pattern object point)))
