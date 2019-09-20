(ns raytracer.tuple-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.tuple :as tuple]
            [raytracer.svector :as svector]
            [raytracer.point :as point]))

(def √2 (/ (Math/sqrt 2) 2))

(deftest test-add
  (testing "adding two tuple"
    (is (t= (tuple/tuple 1 1 6 1)
            (tuple/add (tuple/tuple 3 -2 5 1)
                       (tuple/tuple -2 3 1 0))))
    (is (t= (tuple/tuple 11 102 203 0)
            (tuple/add (tuple/tuple 1 2 3 0)
                       (tuple/tuple 10 100 200 0)))))
  (testing "add more tuples"
    (is (t= (tuple/tuple 8 8 8 8)
            (tuple/add-all (vector (tuple/tuple 1 2 1 2)
                                   (tuple/tuple 2 1 2 1)
                                   (tuple/tuple 1 1 1 1)
                                   (tuple/tuple 4 4 4 4)))))))

(deftest test-sub
  (testing "subtracting two points"
    (is (t= (svector/svector -2 -4 -6)
            (tuple/sub (point/point 3 2 1)
                       (point/point 5 6 7)))))
  (testing "subtracting a vector from a point"
    (is (t= (point/point -2 -4 -6)
            (tuple/sub (point/point 3 2 1)
                       (svector/svector 5 6 7)))))
  (testing "subtracting two vectors"
    (is (t= (svector/svector -2 -4 -6)
            (tuple/sub (svector/svector 3 2 1)
                       (svector/svector 5 6 7)))))
  (testing "subtracting a vector from the zero vector"
    (is (t= (svector/svector -1 2 -3)
            (tuple/sub (svector/svector 0 0 0)
                       (svector/svector 1 -2 3))))))

(deftest test-neg
  (testing "negating a tuple"
    (is (t= (tuple/tuple -1 2 -3 4)
            (tuple/neg (tuple/tuple 1 -2 3 -4))))))

(deftest test-mul
  (testing "dividing a tuple by a scalar"
    (is (t=  (tuple/tuple 1 -2 3 -4)
             (tuple/mul (tuple/tuple 0.5 -1 1.5 -2) 2)))))

(deftest test-div
  (testing "dividing a tuple by a scalar"
    (is (t= (tuple/tuple 0.5 -1 1.5 -2)
            (tuple/div (tuple/tuple 1 -2 3 -4) 2)))))

(deftest test-normalize
  (testing "normalizing a tuple with non zero length"
    (is (t= (tuple/tuple 1 0 0 0)
            (tuple/normalize (tuple/tuple 25 0 0 0))))
    (is (t= (tuple/tuple √2 0 √2 0)
            (tuple/normalize (tuple/tuple 1 0 1 0)))))
  (testing "normalizing a zero length vector"
    (is (t= (tuple/tuple 0 0 0 0)
            (tuple/normalize (tuple/tuple 0 0 0 0)))))
  (testing "normalizing a very small vector"
    (is (t= (tuple/tuple 0 0 0 0)
            (tuple/normalize (tuple/tuple 1e-8 0 3e-7 0))))))

(deftest test-op
  (testing "applying a generic operator"
    (is (t= (tuple/tuple 1 2 3 1)
            (tuple/op (tuple/tuple 1 1000 3 1)
                      (tuple/tuple 100 2 3.5 1)
                      min)))))

(deftest test-any-c?
  (testing "filtering by component"
    (is (tuple/any-c? (tuple/tuple 1 0 0 0) #(> % 0)))
    (is (tuple/any-c? (tuple/tuple 0 1 0 0) #(> % 0)))
    (is (tuple/any-c? (tuple/tuple 0 0 1 0) #(> % 0)))
    (is (not (tuple/any-c? (tuple/tuple 0 0 0 1) #(> % 0))))))
