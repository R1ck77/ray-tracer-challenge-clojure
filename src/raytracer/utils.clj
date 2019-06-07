(ns raytracer.utils
  (:require [raytracer.const :as const]))

(defn zero? [v]
  (< (Math/abs (double v)) const/EPSILON))

(defmacro mmap [f v]
  `(vector
    ~@(map #(list f %) v)))

(defn- expand-comparison
  ([f values carried]
   (if (empty? values)
     carried
     (let [next (first values)
           remaining (rest values)]
       (if (empty? remaining)
         (list 'if (list f next)
               (reverse (conj carried next))
               (reverse carried))
         (list 'if (list f next)
               (expand-comparison f remaining (conj carried next))
               (expand-comparison f remaining carried)))))))

(defmacro mfilter                                                                                                                                                                                                  
  "Unroll the filtering on a sequence of known elements to reproduce the filter behavior"                                                                                                                          
  [f values]                                                                                                                                                                                                       
  (expand-comparison f values '(list)))
