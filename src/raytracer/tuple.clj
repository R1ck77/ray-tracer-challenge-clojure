(ns raytracer.tuple)

(set! *unchecked-math* true)

(defmacro add [& args]
  `(vec (map + ~@args)))

(defn sub [a b]
  (vec (map - a b)))

(defn neg [xn]
  (vec (map - xn)))

(defn div [v s]
  (vec (map #(/ % s) v)))

(defn mul [v s]
  (vec (map #(* % s) v)))






