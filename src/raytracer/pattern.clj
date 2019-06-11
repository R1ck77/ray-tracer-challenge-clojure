(ns raytracer.pattern
  (:require [raytracer.color :as color]
            [raytracer.matrix :as matrix]))

(defprotocol ColorFunction
  (color-at [this point] [this object point]))

(defn- point-in-pattern-space [pattern object point]
  (->> point
       (matrix/transform (:inverse-transform object))
       (matrix/transform (:inverse-transform pattern))))

(defrecord Pattern [inverse-transform function]
  ColorFunction
  (color-at [this point]
    (function this point))
  (color-at [this object point]
    (function this (point-in-pattern-space this object point))))

(defn- create-pattern
  ([function]
     (->Pattern matrix/identity-matrix
                function))
  ([function transform]
     (->Pattern (matrix/invert transform 4)
                function)))

(defrecord SolidPattern [color]
  ColorFunction
  (color-at [this point]
            color)
  (color-at [this object point]
            color))

(defn solid [color]
  (->SolidPattern color))

(defn stripe [color1 color2]
  (create-pattern (fn [_ point]
                    (if (zero? (mod (int (Math/floor (:x point))) 2))
                      color1
                      color2))))

(defn gradient [color1 color2]
  (create-pattern (fn [_ point]
                    (let [x (:x point)
                          p (- x (Math/floor x))]
                      (color/add (color/scale color1 (- 1 x))
                                 (color/scale color2 x))))))

(defn test []
  (create-pattern (fn [_ point]
                    (color/color (:x point)
                                 (:y point)
                                 (:z point)))))

(defn ring [color1 color2]
  (create-pattern (fn [_ point]
                    (let [x (:x point)
                          z (:z point)
                          distance (Math/sqrt (+ (* x x) (* z z)))]
                      (if (zero? (mod (int (Math/floor distance)) 2))
                        color1
                        color2)))))

(defn checker [white black]
  (create-pattern (fn [_ point]
                    (if (zero? (mod (+ (int (Math/floor (+ 2e9 (:x point))))
                                       (int (Math/floor (+ 2e9 (:y point))))
                                       (int (Math/floor (+ 2e9 (:z point))))) 2))
                      white
                      black))))

(defn blend [pattern-a pattern-b]
  (create-pattern (fn [_ point]
                    (color/scale (color/add (color-at pattern-a point)
                                            (color-at pattern-b point))
                                 0.5))))

(defn perturb-pattern [pattern perturbation-f]
  (create-pattern (fn [_ point]
                    (perturbation-f pattern point))
                  (:inverse-transform pattern)))

(defn change-transform [pattern new-transform]
  (create-pattern (:function pattern)
                  new-transform))
