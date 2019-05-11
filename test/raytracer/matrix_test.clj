(ns raytracer.matrix-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer [v= eps= t=]]
            [raytracer.point :as point]
            [raytracer.matrix :as matrix]))

;;; TODO/FIXME trying hard not to use types
(deftest test-construction
  (testing "constructing and inspecting a 4x4 matrix"
    (let [matrix [1 2 3 4 
                  5.5 6.5 7.5 8.5 
                  9 10 11 12 
                  13.5 14.5 15.5 16.5]]
      (is (eps= 1 (matrix/get4 matrix 0 0)))
      (is (eps= 1 (matrix/get4 matrix 0 0)))
      (is (eps= 4 (matrix/get4 matrix 0 3)))
      (is (eps= 5.5 (matrix/get4 matrix 1 0)))
      (is (eps= 7.5 (matrix/get4 matrix 1 2)))
      (is (eps= 11 (matrix/get4 matrix 2 2)))
      (is (eps= 13.5 (matrix/get4 matrix 3 0)))
      (is (eps= 15.5 (matrix/get4 matrix 3 2)))))
  (testing "constructing and inspecting a 2x2 matrix"
    (let [matrix [-3 5
                  1 -2]]
      (is (eps= -3 (matrix/get2 matrix 0 0)))
      (is (eps= 5 (matrix/get2 matrix 0 1)))
      (is (eps= 1 (matrix/get2 matrix 1 0)))
      (is (eps= -2 (matrix/get2 matrix 1 1)))))
  (testing "constructing and inspecting a 3x3 matrix"
    (let [matrix [-3 5 0
                  1 -2 7
                  0 1 1]]
      (is (eps= -3 (matrix/get3 matrix 0 0)))
      (is (eps= -2 (matrix/get3 matrix 1 1)))
      (is (eps= 1 (matrix/get3 matrix 2 2)))
      (is (eps= 7 (matrix/get3 matrix 1 2))))))

(deftest test-equality
  (testing "equal matrix"
    (is (v= [1 2 3 4
             5 6 7 8
             9 8 7 6
             5 4 3 2]
            [1 2 3 4
             5 6 7 8
             9 8 7 6
             5 4 3 2])))
  (testing "different matrix"
    (is (not (v= [1 2 3 4
                  5 6 7 8
                  9 8 7 6
                  5 4 3 2]
                 [1 2 3 4
                  5 6 7 8.6
                  9 8 7 6
                  5 4 3 2])))
    (is (not (v= [1 2 3 4
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
      (is (v= expected (matrix/mul4 m1 m2)))))
  (testing "the identity matrix is the identity element"
    (let [m1 [1 2 3 4
              5 6 7 8
              9 8 7 6
              5 4 3 2]
          expected [20 22 50 48
                    44 54 114 108
                    40 58 110 102
                    16 26 46 42]]
      (is (v= m1 (matrix/mul4 m1 matrix/identity-matrix))))))

(deftest test-transform
  (testing "matrix multiplied by a tuple"
    (let [a [1 2 3 4
             2 4 4 2
             8 6 4 1
             0 0 0 1]
          b (point/point 1 2 3)]
      (is (t= (point/point 18 24 33) (matrix/transform a b))))))

(deftest test-transpose
  (testing "transposing a matrix"
    (is (v= [ 1  2  3  4
             5  6  7  8
             9 10 11 12
             13 14 15 16]
            (matrix/transpose [1 5  9 13
                               2 6 10 14
                               3 7 11 15
                               4 8 12 16]))))
  (testing "transposing the identity yields identity"
    (is (v= matrix/identity-matrix
            (matrix/transpose matrix/identity-matrix)))))

(deftest test-det2
  (testing "2x2 matrix determinant"
    (is (eps= 17 (matrix/det2 [1 5 -3 2])))))

(deftest test-submatrix
  (testing "a submatrix of a 3x3 matrix is a 2x2 matrix"
    (is (v= [-3 2
             0 6]
            (matrix/submatrix [ 1 5  0
                               -3 2  7
                               0 6 -3] 3 0 2))))
  (testing "a submatrix of a 4x4 matrix is a 3x3 matrix"
    (is (v= [1 2 3
             4 5 6
             7 8 9]
            (matrix/submatrix [1  42  2  3
                               4  42  5  6
                               42 42 42 42
                               7  42  8  9] 4 2 1)))))

(deftest test-minor
  (testing "calculating a minor of a 3x3 matrix"
    (is (eps= 25 (matrix/minor [3 5 0 2 -1 -7 6 -1 5] 3 1 0)))))

(deftest test-cofactor
  (testing "cofactors of 3x3 matrix"
    (let [m [3  5  0
             2 -1 -7
             6 -1  5]]
      (is (eps= -12 (matrix/cofactor m 3 0 0)))
      (is (eps= -25 (matrix/cofactor m 3 1 0))))))

(deftest test-determinant
  (testing "calculating the determinant of a 3x3 matrix"
    (is (eps= -196 (matrix/det [ 1 2  6
                                -5 8 -4
                                2 6  4] 3))))
  (testing "calculating the determinant of a 4x4 matrix"
    (is (eps= -4071 (matrix/det [-2 -8  3  5
                                 -3  1  7  3
                                 1  2 -9  6
                                 -6  7  7 -9] 4)))))

(deftest test-is-invertible?
  (testing "positive case"
    (is (matrix/is-invertible? [6  4 4  4
                                5  5 7  6
                                4 -9 3 -7
                                9  1 7 -6] 4)))
  (testing "negative case"
    (is (not (matrix/is-invertible? [-4  2 -2 -3
                                     9  6  2  6
                                     0 -5  1 -5
                                     0  0  0  0] 4)))))

(deftest test-invert
  (testing "inversion 1"
    (let [A [-5  2  6 -8
             1 -5  1  8
             7  7 -6 -7
             1 -3  7  4]]
      (is (v= matrix/identity-matrix 
              (matrix/mul4 A (matrix/invert A 4))))))
  (testing "inversion 2"
    (let [A [8 -5 9 2
             7 5 6 1
             -6 0 9 6
             -3 0 -9 -4]]
      (is (v= matrix/identity-matrix 
              (matrix/mul4 A (matrix/invert A 4))))))
  (testing "inversion 3"
    (let [A [3 -9 7 3
             3 -8 2 -9
             -4 4 4 1
             -6 5 -1 1]
          B [8 2 2 2
             3 -1 7 0
             7 0 5 4
             6 -2 0 5]
          C (matrix/mul4 A B)]
      (is (v= A
              (matrix/mul4 C (matrix/invert B 4)))))))
