(ns raytracer.utils
  (:require [raytracer.const :as const]))

(defn zero? [v]
  (< (Math/abs (double v)) const/EPSILON))

(defmacro quick-map
  "Non composable macro-version of map.

  Useful for performance critical code, if the size of the collection is small."
  [f v]
  `(vector
    ~@(map #(list f %) v)))

(defn- map-filter-f
  ([map-f pred-f values carried]
   (if (empty? values)
     carried
     (let [next (first values)
           remaining (rest values)]
       (if (empty? remaining)
         (list 'if (list pred-f next)
               (reverse (conj carried (list map-f next)))
               (reverse carried))
         (list 'if (list pred-f next)
               (map-filter-f map-f pred-f remaining (conj carried (list map-f next)))
               (map-filter-f map-f pred-f remaining carried)))))))

(defmacro map-filter
  "Loose macro equivalent of (map mapf (filter pred-f values))

  Some performance gain for repeated application on short collections (size < 8)"
  [map-f pred-f values]                                                                                                                                                                                                       
  (map-filter-f map-f pred-f values '(list)))
