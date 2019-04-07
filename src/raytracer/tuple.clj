(ns raytracer.tuple)

(set! *unchecked-math* true)

(defn add [a b]
  (vec (map + a b)))

(defn sub [a b]
  (vec (map - a b)))

(defn neg [xn]
  (vec (map - xn)))

(defn div [v s]
  (vec (map #(/ % s) v)))







