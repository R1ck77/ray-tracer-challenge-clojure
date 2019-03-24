(ns raytracer.tuples)

(def eps 1e-8)

(defn eps= [a b]
  (< (Math/abs (double (- a b))) eps))

(defn eps3= [[a1 a2 a3] [b1 b2 b3]]
  (and (eps= a1 b1)
       (eps= a2 b2)
       (eps= a3 b3)))

(defn eps4= [[a1 a2 a3 a4] [b1 b2 b3 b4]]
  (and (eps= a1 b1)
       (eps= a2 b2)
       (eps= a3 b3)
       (eps= a4 b4)))

(defn add [a b]
  (vec (map + a b)))

(defn sub [a b]
  (vec (map - a b)))

(defn neg [xn]
  (vec (map - xn)))






