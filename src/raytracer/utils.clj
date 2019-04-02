(ns raytracer.utils)

(def eps 1e-6)

(defn zero? [v]
  (< (Math/abs (double v)) eps))

