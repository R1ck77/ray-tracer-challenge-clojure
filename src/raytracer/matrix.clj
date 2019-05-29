(ns raytracer.matrix
  (:require [raytracer.tuple :as tuple]))

(def ^:private max-error 0.0001)

(def identity-matrix (double-array [1 0 0 0
                                    0 1 0 0
                                    0 0 1 0
                                    0 0 0 1]))

(defn get-n [^doubles matrix n i j]
  (aget matrix (+ j (* n i))))

(defn get4 [matrix i j]
  (get-n matrix 4 i j))

(defn get3 [matrix i j]
  (get-n matrix 3 i j))

(defn get2 [matrix i j]
  (get-n matrix 2 i j))

(defmacro cell4 [a row column columns]
  `(aget ~a ~(+ column (* columns row))))

(defmacro mul4-cell [a b row column b-columns index]
  `(* (cell4 ~a ~row ~index 4)
     (cell4 ~b ~index ~column ~b-columns)))

(defmacro rowp4 [a b row column b-columns]
  `(+ (mul4-cell ~a ~b ~row ~column ~b-columns 0)
      (mul4-cell ~a ~b ~row ~column ~b-columns 1)
      (mul4-cell ~a ~b ~row ~column ~b-columns 2)
      (mul4-cell ~a ~b ~row ~column ~b-columns 3)))

(defmacro mul4-macro [a b]
  `(let [a# ~a
         b# ~b]
     (double-array
      (vector (rowp4 a# b# 0 0 4) (rowp4 a# b# 0 1 4) (rowp4 a# b# 0 2 4) (rowp4 a# b# 0 3 4)
              (rowp4 a# b# 1 0 4) (rowp4 a# b# 1 1 4) (rowp4 a# b# 1 2 4) (rowp4 a# b# 1 3 4)
              (rowp4 a# b# 2 0 4) (rowp4 a# b# 2 1 4) (rowp4 a# b# 2 2 4) (rowp4 a# b# 2 3 4)
              (rowp4 a# b# 3 0 4) (rowp4 a# b# 3 1 4) (rowp4 a# b# 3 2 4) (rowp4 a# b# 3 3 4)))))

(defn mul4 [^doubles a ^doubles b]
  (mul4-macro a b))

(defmacro line-vector-prod [m v row]
  `(+ (* (cell4 ~m ~row 0 4) (:x ~v))
      (* (cell4 ~m ~row 1 4) (:y ~v))
      (* (cell4 ~m ~row 2 4) (:z ~v))
      (* (cell4 ~m ~row 3 4) (:w ~v))))

(defmacro transform-macro [m v]
  `(let [m# ~m
         v# ~v]
     (tuple/tuple (line-vector-prod m# v# 0)
                  (line-vector-prod m# v# 1)
                  (line-vector-prod m# v# 2)
                  (line-vector-prod m# v# 3))))

(defn transform [^doubles m v]
  (transform-macro m v))

(defn transpose [^doubles m]
  (double-array
   (reduce #(conj % (aget m %2)) [] [0 4 8 12
                                     1 5 9 13
                                     2 6 10 14
                                     3 7 11 15])))

(defn cells-indices-seq [n]
  (for [i (range n) j (range n)]
    (vector i j)))

(defn submatrix
  "Compute the submatrix of a matrix of size n

  Probably on the slow side, but it's irrelevant given the number of inversions the ray tracer does"
  [m n row column]
  (double-array
   (let [get-f #(get-n m n % %2)]
     (map (fn [[i j]]
            (get-f i j))
          (filter (fn [[i j]]
                    (and (not= i row) (not= j column)))
                  (cells-indices-seq n))))))

(def det)

(defn minor [m n row column]
  (det (submatrix m n row column) (dec n)))

(defn cofactor [m n row column]
  (* (if (odd? (+ row column)) -1 1)
     (minor m n row column)))

(defn det2 [^doubles m]
  (- (* (aget m 0) (aget m 3))
     (* (aget m 1) (aget m 2))))

(defn- det-n [m n]
  (apply + (map #(* (get-n m n 0 %)
                    (cofactor m n 0 %)) (range n))))

(defn det [m n]
  (case (int n)
    1 (first m)
    2 (det2 m)
    (det-n m n)))

(defn is-invertible? [m n]
  (> (Math/abs (double (det m n))) max-error))

(defn- cofactor-matrix [m n]
  (let [cofactor-f (partial cofactor m n)]
    (double-array
     (map (fn [[i j]]
            (cofactor-f i j))
          (cells-indices-seq n)))))

(defn invert [m n]
  (double-array
   (map float (map #(/ % (det m n)) (transpose (cofactor-matrix m n ))))))
