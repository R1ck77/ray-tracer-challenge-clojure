(ns raytracer.test-utils
  (:require [clojure.test :refer :all]))

(def ^:dynamic *eps* 1e-6)
(def ^:private max-percent-error 0.001)

(defn- absolute-error [a b]
  (< (Math/abs (double (- a b)))
     *eps*))

(defn- relative-error [a b]
  (< (Math/abs (/ (double (- a b)) a))
     max-percent-error))

(defn eps= [a b]
  (if (< (Math/abs (double a)) *eps*)
    (absolute-error a b)
    (relative-error a b)))

(defn v= [a b]
  (and (= (count a) (count b))
       (reduce #(and % %2)
               true
               (map eps= a b))))

(deftest test-eps=
  (testing "general cases"
    (is (eps= 1 1))
    (is (eps= 12 12.0000000000000000001))
    (is (eps= (/ 1 3) (/ (* 3 (/ 1 3)) 3)))))

(deftest test-v=
  (testing "returns false for tuples of different size"
    (is (not (v= [1 2 3] [1 2 3 4])))
    (is (not (v= [1 2 3 4] [1 2 3]))))
  (testing "returns true for similar tuples of same size"
    (is (v= [4.3 2.3 6.7 1.3] [4.3 2.3 6.7 1.3]))
    (let [small-eps 1e-7]
      (is (v= [4.3 2.3 6.7 1] (map #(+ small-eps %) [4.3 2.3 6.7 1])))
      (is (v= [0 0 0 0] [small-eps small-eps small-eps small-eps]))))
  (testing "v= returns false for tuple that are too dissimilar"
    (is (not (v= [0 0 0 0] [*eps* 0 0 0])))
    (is (not (v= [0 0 0 0] [0 *eps* 0 0])))
    (is (not (v= [0 0 0 0] [0 0 *eps* 0])))
    (is (not (v= [0 0 0 0] [0 0 0 *eps*])))))

(defn t= [t1 t2]
  (and (eps= (:x t1) (:x t2))
       (eps= (:y t1) (:y t2))
       (eps= (:z t1) (:z t2))
       (eps= (:w t1) (:w t2))))

(defn c= [c1 c2]
  (and (eps= (:r c1) (:r c2))
       (eps= (:g c1) (:g c2))
       (eps= (:b c1) (:b c2))))

(defmacro with-temp-file [name & forms]
  `(let [~name (java.io.File/createTempFile "pre" ".suff")]
     (try 
       ~@forms
       (finally
         (.delete ~name)))))
