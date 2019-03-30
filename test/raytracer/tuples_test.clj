(ns raytracer.tuples-test
  (:require [clojure.test :refer :all]
            [raytracer.tuples :refer :all]
            [raytracer.svector :refer [svector]]
            [raytracer.point :refer [point]]))

(deftest test-eps=
  (testing "general cases"
    (is (eps= 1 1))
    (is (eps= 12 12.0000000000000000001))
    (is (eps= (/ 1 3) (/ (* 3 (/ 1 3)) 3))))
  (testing "edge cases"
    (is (eps= 1 (+ 1 (/ eps 2)) ))
    (is (eps= 1 (- 1 (/ eps 2)) ))))


(deftest test-eps4=
  (testing "eps4= returns true for very similar tuples"
    (is (eps4= [4.3 2.3 6.7 1.3] [4.3 2.3 6.7 1.3]))
    (let [small-eps (/ eps 100)]
      (is (eps4= [4.3 2.3 6.7 1] (map #(+ small-eps %) [4.3 2.3 6.7 1])))
      (is (eps4= [0 0 0 0] [small-eps small-eps small-eps small-eps]))))
  (testing "eps4= returns false for tuples that are too dissimilar"
    (is (not (eps4= [0 0 0 0] [eps 0 0 0])))
    (is (not (eps4= [0 0 0 0] [0 eps 0 0])))
    (is (not (eps4= [0 0 0 0] [0 0 eps 0])))
    (is (not (eps4= [0 0 0 0] [0 0 0 eps])))))

(deftest test-add
  (testing "adding two tuples"
    (is (eps4= [1 1 6 1] (add [3 -2 5 1] [-2 3 1 0])))
    (is (eps4= [11 102 203 0] (add [1 2 3 0] [10 100 200 0])))))

(deftest test-sub
  (testing "subtracting two points"
    (is (eps4= (svector -2 -4 -6)
            (sub (point 3 2 1)
                 (point 5 6 7)))))
  (testing "subtracting a vector from a point"
    (is (eps4= (point -2 -4 -6)
            (sub (point 3 2 1)
                 (svector 5 6 7)))))
  (testing "subtracting two vectors"
    (is (eps4= (svector -2 -4 -6)
            (sub (svector 3 2 1)
                 (svector 5 6 7)))))
    (testing "subtracting a vector from the zero vector"
      (is (eps4= (svector -1 2 -3)
              (sub (svector 0 0 0) (svector 1 -2 3))))))

(deftest test-neg
  (testing "negating a tuple"
    (is (eps4= [-1 2 -3 4]
            (neg (vector 1 -2 3 -4))))))


(deftest test-div
  (testing "dividing a tuple by a scalar"
    (eps4= [0.5 -1 1.5 -2]
        (div [1 -2 3 -4] 2))))

