(ns raytracer.tuples-test
  (:require [clojure.test :refer :all]
            [raytracer.tuples :refer :all]))

(deftest test-is-about?
  (testing "general cases"
    (is (is-about? 1 1))
    (is (is-about? 12 12.0000000000000000001))
    (is (is-about? (/ 1 3) (/ (* 3 (/ 1 3)) 3))))
  (testing "edge cases"
    (is (is-about? 1 (+ 1 (/ eps 2)) ))
    (is (is-about? 1 (- 1 (/ eps 2)) ))))

(deftest test-svector?
  (testing "svector? returns true for tuples when w is 0"
    (is (svector? [3.5 1.2 5.4 0.0]))
    (is (svector? [32 12.0 -19.0 (/ eps 2)]))
    (is (svector? [3.5 1.2 5.4 (- (/ eps 2))])))
  (testing "svector? returns false for tuples with w not 0"
    (is (not (svector? [3.5 1.2 5.4 1])))
    (is (not (svector? [32 12.0 -19.0 eps])))
    (is (not (svector? [3.5 1.2 5.4 eps])))))

(deftest test-point?
  (testing "point? returns true for tuples when w is 1"
    (is (point? [3.5 1.2 5.4 1.0]))
    (is (point? [32 12.0 -19.0 (+ 1 (/ eps 2))]))
    (is (point? [3.5 1.2 5.4 (- 1 (/ eps 2))])))
  (testing "point? returns false for tuples with w not 1"
    (is (not (point? [3.5 1.2 5.4 0])))
    (is (not (point? [32 12.0 -19.0 (+ 1 (* 2 eps))])))
    (is (not (point? [3.5 1.2 5.4 (- 1 (* 2  eps))])))))

(deftest test-point
  (testing "point creates a tuple with w == 1"
    (is (point? (point 1 2 3)))
    (is (not (svector? (point 4 1 2))))))

(deftest test-vector
  (testing "svector creates a tuple with w == 0"
    (is (svector? (svector 1 2 3)))
    (is (not (point? (svector 4 1 2))))))

(deftest test-eq
  (testing "eq returns true for very similar tuples"
    (is (eq [4.3 2.3 6.7 1.3] [4.3 2.3 6.7 1.3]))
    (let [small-eps (/ eps 100)]
      (is (eq [4.3 2.3 6.7 1] (map #(+ small-eps %) [4.3 2.3 6.7 1])))
      (is (eq [0 0 0 0] [small-eps small-eps small-eps small-eps]))))
  (testing "eq returns false for tuples that are too dissimilar"
    (is (not (eq [0 0 0 0] [eps 0 0 0])))
    (is (not (eq [0 0 0 0] [0 eps 0 0])))
    (is (not (eq [0 0 0 0] [0 0 eps 0])))
    (is (not (eq [0 0 0 0] [0 0 0 eps])))))

(deftest test-add
  (testing "adding two tuples"
    (is (eq [1 1 6 1] (add [3 -2 5 1] [-2 3 1 0])))
    (is (eq [11 102 203 0] (add [1 2 3 0] [10 100 200 0])))))

(deftest test-sub
  (testing "subtracting two points"
    (is (eq (svector -2 -4 -6)
            (sub (point 3 2 1)
                 (point 5 6 7)))))
  (testing "subtracting a vector from a point"
    (is (eq (point -2 -4 -6)
            (sub (point 3 2 1)
                 (svector 5 6 7)))))
  (testing "subtracting two vectors"
    (is (eq (svector -2 -4 -6)
            (sub (svector 3 2 1)
                 (svector 5 6 7))))))
