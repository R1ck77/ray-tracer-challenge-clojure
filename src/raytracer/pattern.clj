(ns raytracer.pattern)

(defn stripe [white black]
  {:a white
   :b black
   :stripe-at (fn [pattern point]
                (if (zero? (mod (int (Math/floor (first point))) 2))
                  (:a pattern)
                  (:b pattern)))})
