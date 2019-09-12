(ns raytracer.pattern
  (:require [raytracer.color :as color]
            [raytracer.matrix :as matrix]
            [raytracer.shapes.shared :as shared]
            [raytracer.shapes.placement :as placement]))

(defprotocol ColorFunction
  (color-at [this point] [this object point]))

(defn- point-in-pattern-space [pattern object point]
  (->> point
       (matrix/transform (-> object shared/get-placement placement/get-inverse-transform))
       (matrix/transform (:inverse-transform pattern))))

(defrecord Pattern [inverse-transform function function-data]
  ColorFunction
  (color-at [this point]
    (function function-data  point))
  (color-at [this object point]
    (function function-data (point-in-pattern-space this object point))))

(defn- create-pattern
  ([function function-data]
     (->Pattern matrix/identity-matrix
                function function-data))
  ([function function-data transform]
     (->Pattern (matrix/invert transform 4)
                function function-data)))

(defn- solid-color-function [function-data _]
  (:color function-data))

(defn solid [color]
  (create-pattern solid-color-function
                  {:color color}))

(defn- stripe-color-function [function-data point]
  (if (zero? (mod (int (Math/floor (:x point))) 2))
    (:color1 function-data)
    (:color2 function-data)))

(defn stripe [color1 color2]
  (create-pattern stripe-color-function
                  {:color1 color1, :color2 color2}))

(defn- gradient-color-function [function-data point]
  (let [x (:x point)
        p (- x (Math/floor x))]
    (color/add (color/scale (:color1 function-data) (- 1 x))
               (color/scale (:color2 function-data) x))))

(defn gradient [color1 color2]
  (create-pattern gradient-color-function
                  {:color1 color1, :color2 color2}))

(defn- test-color-function [_ point]
  (color/color (:x point)
               (:y point)
               (:z point)))

(defn test-pattern []
  (create-pattern test-color-function nil))

(defn- ring-color-function [function-data point]
  (let [x (:x point)
        z (:z point)
        distance (Math/sqrt (+ (* x x) (* z z)))]
    (if (zero? (mod (int (Math/floor distance)) 2))
      (:color1 function-data)
      (:color2 function-data))))

(defn ring [color1 color2]
  (create-pattern ring-color-function
                  {:color1 color1, :color2 color2}))

(defn- checker-color-function [function-data point]
  (if (zero? (mod (+ (int (Math/floor (+ 2e9 (:x point))))
                     (int (Math/floor (+ 2e9 (:y point))))
                     (int (Math/floor (+ 2e9 (:z point))))) 2))
    (:color1 function-data)
    (:color2 function-data)))

(defn checker [white black]
  (create-pattern checker-color-function
                  {:color1 white, :color2 black}))

(defn blend-color-function [function-data point])

(defn blend [pattern-a pattern-b]
  (create-pattern (fn [function-data point]
                    (color/scale
                     (color/add (color-at (:pattern-a function-data) point)
                                (color-at (:pattern-b function-data) point))
                     0.5))
                  {:pattern-a pattern-a, :pattern-b pattern-b}))

(defn perturb-pattern [pattern perturbation-f]
  (create-pattern (fn [_ point]
                    (perturbation-f pattern point))
                  (:inverse-transform pattern)))

(defn change-transform [pattern new-transform]
  (create-pattern (:function pattern)
                  (:function-data pattern)
                  new-transform))
