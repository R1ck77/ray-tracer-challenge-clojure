(ns raytracer.tuple-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.tuple :as tuple]
            [raytracer.svector :refer [svector]]
            [raytracer.point :refer [point]]))

(deftest test-add
  (testing "adding two tuple"
    (is (v= [1 1 6 1] (tuple/add [3 -2 5 1] [-2 3 1 0])))
    (is (v= [11 102 203 0] (tuple/add [1 2 3 0] [10 100 200 0])))))

(deftest test-sub
  (testing "subtracting two points"
    (is (v= (svector -2 -4 -6)
            (tuple/sub (point 3 2 1)
                 (point 5 6 7)))))
  (testing "subtracting a vector from a point"
    (is (v= (point -2 -4 -6)
            (tuple/sub (point 3 2 1)
                 (svector 5 6 7)))))
  (testing "subtracting two vectors"
    (is (v= (svector -2 -4 -6)
            (tuple/sub (svector 3 2 1)
                 (svector 5 6 7)))))
  (testing "subtracting a vector from the zero vector"
    (is (v= (svector -1 2 -3)
            (tuple/sub (svector 0 0 0) (svector 1 -2 3))))))

(deftest test-neg
  (testing "negating a tuple"
    (is (v= [-1 2 -3 4]
            (tuple/neg (vector 1 -2 3 -4))))))

(deftest test-mul
  (testing "dividing a tuple by a scalar"
    (v=  [1 -2 3 -4]
         (tuple/mul [0.5 -1 1.5 -2]2))))

(deftest test-div
  (testing "dividing a tuple by a scalar"
    (v= [0.5 -1 1.5 -2]
        (tuple/div [1 -2 3 -4] 2))))

