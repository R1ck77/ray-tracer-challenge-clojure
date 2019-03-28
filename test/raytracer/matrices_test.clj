(ns raytracer.matrices-test
  (:require [clojure.test :refer :all]
            [raytracer.tuples :refer [eps= eps4=]]
            [raytracer.matrices :as matrices]))

;;; TODO/FIXME trying hard not to use types
(deftest test-construction
  (testing "constructing and inspecting a 4x4 matrix"
    (let [matrix [1 2 3 4 
                  5.5 6.5 7.5 8.5 
                  9 10 11 12 
                  13.5 14.5 15.5 16.5]]
      (is (eps= 1 (matrices/get4 matrix 0 0)))
      (is (eps= 1 (matrices/get4 matrix 0 0)))
      (is (eps= 4 (matrices/get4 matrix 0 3)))
      (is (eps= 5.5 (matrices/get4 matrix 1 0)))
      (is (eps= 7.5 (matrices/get4 matrix 1 2)))
      (is (eps= 11 (matrices/get4 matrix 2 2)))
      (is (eps= 13.5 (matrices/get4 matrix 3 0)))
      (is (eps= 15.5 (matrices/get4 matrix 3 2)))))
  (testing "constructing and inspecting a 2x2 matrix"
    (let [matrix [-3 5
                  1 -2]]
      (is (eps= -3 (matrices/get2 matrix 0 0)))
      (is (eps= 5 (matrices/get2 matrix 0 1)))
      (is (eps= 1 (matrices/get2 matrix 1 0)))
      (is (eps= -2 (matrices/get2 matrix 1 1)))))
  (testing "constructing and inspecting a 3x3 matrix"
    (let [matrix [-3 5 0
                  1 -2 7
                  0 1 1]]
      (is (eps= -3 (matrices/get3 matrix 0 0)))
      (is (eps= -2 (matrices/get3 matrix 1 1)))
      (is (eps= 1 (matrices/get3 matrix 2 2)))
      (is (eps= 7 (matrices/get3 matrix 1 2))))))

(deftest test-equality
  (testing "equal matrices"
    (is (matrices/m= [1 2 3 4
                      5 6 7 8
                      9 8 7 6
                      5 4 3 2]
                     [1 2 3 4
                      5 6 7 8
                      9 8 7 6
                      5 4 3 2])))
  (testing "different matrices"
    (is (not (matrices/m= [1 2 3 4
                           5 6 7 8
                           9 8 7 6
                           5 4 3 2]
                          [1 2 3 4
                           5 6 7 8.6
                           9 8 7 6
                           5 4 3 2])))
    (is (not (matrices/m= [1 2 3 4
                           5 6 7 8
                           9 8 7 6
                           5 4 3 2]
                          [2 3 4 5
                           6 7 8 9
                           8 7 6 5
                           4 3 2 1])))))

(deftest test-multiply
  (testing "4x4 multiplication"
    (let [m1 [1 2 3 4
             5 6 7 8
             9 8 7 6
             5 4 3 2]
          m2 [-2 1 2 3
             3 2 1 -1
             4 3 6 5
             1 2 7 8]
          expected [20 22 50 48
                    44 54 114 108
                    40 58 110 102
                    16 26 46 42]]
      (is (matrices/m= expected (matrices/mul4 m1 m2))))))

(deftest test-transform
  (testing "matrix multiplied by a tuple"
    (let [a [1 2 3 4
             2 4 4 2
             8 6 4 1
             0 0 0 1]
          b [1 2 3 1]]
      (is (eps4= [18 24 33 1]
                 (matrices/transform a b))))))
