(ns raytracer.tuples)

(def eps 1e-8)

(defn is-about? [a b]
  (< (Math/abs (double (- a b))) eps))

(defn svector? [tuple]
  (is-about? (nth tuple 3) 0))

(defn point? [tuple]
  (is-about? (nth tuple 3) 1))

(defn svector [x y z]
  (vector x y z 0))

(defn point [x y z]
  (vector x y z 1))

(defn eq [[x1 y1 z1 w1] [x2 y2 z2 w2]]
  (and (is-about? x1 x2)
       (is-about? y1 y2)
       (is-about? z1 z2)
       (is-about? w1 w2)))

(defn add [a b]
  (vec (map + a b)))

;;; TODO/FIXME this is fishy
;;; errors could accumulate and the result could be neither a vector or a point.
;;; Should I keep track of the vector/point status somehow?
(defn sub [a b]
  (vec (map - a b)))
