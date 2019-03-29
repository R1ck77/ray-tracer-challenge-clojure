(ns raytracer.matrices
  (:require [raytracer.tuples :refer [eps=]]))

(def identity-matrix [1 0 0 0
                      0 1 0 0
                      0 0 1 0
                      0 0 0 1])

(defn get-n [matrix n i j]
  (get matrix (+ j (* n i))))

(defn get4 [matrix i j]
  (get-n matrix 4 i j))

(defn get3 [matrix i j]
  (get-n matrix 3 i j))

(defn get2 [matrix i j]
  (get-n matrix 2 i j))

(defn m= [a b]
  (reduce #(and % %2) (map eps= a b)))

(defmacro cell4 [a row column columns]
  `(nth ~a ~(+ column (* columns row))))

(defmacro mul4-cell [a b row column b-columns index]
  `(* (cell4 ~a ~row ~index 4)
     (cell4 ~b ~index ~column ~b-columns)))

(defmacro rowp4 [a b row column b-columns]
  `(+ (mul4-cell ~a ~b ~row ~column ~b-columns 0)
      (mul4-cell ~a ~b ~row ~column ~b-columns 1)
      (mul4-cell ~a ~b ~row ~column ~b-columns 2)
      (mul4-cell ~a ~b ~row ~column ~b-columns 3)))

(defmacro mul4 [a b]
  `(let [a# ~a
         b# ~b]
     (vector (rowp4 a# b# 0 0 4) (rowp4 a# b# 0 1 4) (rowp4 a# b# 0 2 4) (rowp4 a# b# 0 3 4)
             (rowp4 a# b# 1 0 4) (rowp4 a# b# 1 1 4) (rowp4 a# b# 1 2 4) (rowp4 a# b# 1 3 4)
             (rowp4 a# b# 2 0 4) (rowp4 a# b# 2 1 4) (rowp4 a# b# 2 2 4) (rowp4 a# b# 2 3 4)
             (rowp4 a# b# 3 0 4) (rowp4 a# b# 3 1 4) (rowp4 a# b# 3 2 4) (rowp4 a# b# 3 3 4))))

(defmacro line-vector-prod [m v row]
  `(+ (* (cell4 ~m ~row 0 4) (nth ~v 0))
      (* (cell4 ~m ~row 1 4) (nth ~v 1))
      (* (cell4 ~m ~row 2 4) (nth ~v 2))
      (* (cell4 ~m ~row 3 4) (nth ~v 3))))

(defmacro transform [m v]
  `(let [m# ~m
         v# ~v]
     (vector (line-vector-prod m# v# 0)
             (line-vector-prod m# v# 1)
             (line-vector-prod m# v# 2)
             (line-vector-prod m# v# 3))))

(defn transpose [m]
  (reduce #(conj % (nth m %2)) [] [0 4 8 12
                                   1 5 9 13
                                   2 6 10 14
                                   3 7 11 15]))

(defn det [m]
  (- (* (nth m 0) (nth m 3))
     (* (nth m 1) (nth m 2))))

(defn cells-indices-seq [n]
  (for [i (range n) j (range n)]
    (vector i j)))

;;; TODO/FIXME what a nice place for an object this would be…
(defn submatrix [m n row column]
  (let [get-f #(get-n m n % %2)]
    (vec
     (map #(apply get-f %)
          (filter (fn [[i j]]
                    (and (not= i row) (not= j column)))
                  (cells-indices-seq n))))))

(defn minor [m n row column]
  (det (submatrix m n row column)))