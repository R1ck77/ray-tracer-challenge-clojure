(ns raytracer.test-utils
  (:require [clojure.test :refer :all]))

(def ^:private eps 1e-6)
(def ^:private max-percent-error 0.001)

(defn- absolute-error [a b]
  (< (Math/abs (double (- a b)))
     eps))

(defn- relative-error [a b]
  (< (/ (Math/abs (double (- a b))) a)
     max-percent-error))

(defn eps= [a b]
  (if (< (Math/abs (double a)) eps)
    (absolute-error a b)
    (relative-error a b)))

(defn m= [a b]
  (reduce #(and % %2) (map eps= a b)))

(defn eps2= [[a1 a2] [b1 b2]]
  (and (eps= a1 b1)
       (eps= a2 b2)))

(defn eps3= [[a1 a2 a3] [b1 b2 b3]]
  (and (eps= a1 b1)
       (eps= a2 b2)
       (eps= a3 b3)))

(defn eps4= [[a1 a2 a3 a4] [b1 b2 b3 b4]]
  (and (eps= a1 b1)
       (eps= a2 b2)
       (eps= a3 b3)
       (eps= a4 b4)))


(deftest test-eps=
  (testing "general cases"
    (is (eps= 1 1))
    (is (eps= 12 12.0000000000000000001))
    (is (eps= (/ 1 3) (/ (* 3 (/ 1 3)) 3)))))

(deftest test-eps4=
  (testing "eps4= returns true for very similar tuple"
    (is (eps4= [4.3 2.3 6.7 1.3] [4.3 2.3 6.7 1.3]))
    (let [small-eps 1e-7]
      (is (eps4= [4.3 2.3 6.7 1] (map #(+ small-eps %) [4.3 2.3 6.7 1])))
      (is (eps4= [0 0 0 0] [small-eps small-eps small-eps small-eps]))))
  (testing "eps4= returns false for tuple that are too dissimilar"
    (is (not (eps4= [0 0 0 0] [eps 0 0 0])))
    (is (not (eps4= [0 0 0 0] [0 eps 0 0])))
    (is (not (eps4= [0 0 0 0] [0 0 eps 0])))
    (is (not (eps4= [0 0 0 0] [0 0 0 eps])))))
