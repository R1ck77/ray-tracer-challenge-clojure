(ns raytracer.tuple-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.tuple :refer :all]
            [raytracer.svector :refer [svector]]
            [raytracer.point :refer [point]]))

(deftest test-add
  (testing "adding two tuple"
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

