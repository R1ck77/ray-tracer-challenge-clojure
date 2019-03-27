(ns raytracer.matrices
  (:require [raytracer.tuples :refer [eps=]]))

(defn get4 [matrix i j]
  (get matrix (+ j (* 4 i))))

(defn get3 [matrix i j]
  (get matrix (+ j (* 3 i))))

(defn get2 [matrix i j]
  (get matrix (+ j (* 2 i))))

(defn m= [a b]
  (reduce #(and % %2) (map eps= a b)))

(defmacro cell4 [a row column]
  `(nth ~a (+ ~column (* 4 ~row))))

(defmacro mul4-cell [a b row column index]
  `(* (cell4 a index column)
     (cell4 b row index)))

(defmacro rowp4 [a b row column]
  `(+ (mul4-cell ~a ~b ~row ~column 0)
      (mul4-cell ~a ~b ~row ~column 1)
      (mul4-cell ~a ~b ~row ~column 2)
      (mul4-cell ~a ~b ~row ~column 3)))

(defmacro mul4 [a b]
  (vec (rowp4 a b 0 0) (rowp4 a b 0 1) (rowp4 a b 0 1) (rowp4 a b 0 1)
       (rowp4 a b 1 0) (rowp4 a b 1 1) (rowp4 a b 1 1) (rowp4 a b 1 1)
       (rowp4 a b 2 0) (rowp4 a b 2 1) (rowp4 a b 2 1) (rowp4 a b 2 1)
       (rowp4 a b 3 0) (rowp4 a b 3 1) (rowp4 a b 3 1) (rowp4 a b 3 1)))


