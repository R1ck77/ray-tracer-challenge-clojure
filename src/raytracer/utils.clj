(ns raytracer.utils
  (:require [raytracer.const :as const]))

(defn zero? [v]
  (< (Math/abs (double v)) const/EPSILON))

(defmacro mmap [f v]
  `(vector
    ~@(map #(list f %) v)))

(defn- expand-comparison
  ([f values carried]
   (let [next (first values)
         remaining (rest values)]
     (if (empty? remaining)
       ;;; last one
       (list 'if (list f next)
               (conj carried next)
               carried)
       ;;; more to come
       (list 'if (list f next)
               (templ f remaining (conj carried next))
               (templ f remaining carried))))))

(defmacro mfilter
  "Unroll the filtering on a sequence of known elements to reproduce the filter behavior"
  [f values]
  (templ f values []))
