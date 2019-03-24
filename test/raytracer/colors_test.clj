(ns raytracer.colors-test
  (:require [clojure.test :refer :all]
            [raytracer.tuples :refer [eps3=]]
            [raytracer.colors :refer :all]))

(deftest test-add-color
  (testing "adding colors"
    (is (eps3= [1.6 0.7 1.0]
                   (add [0.9 0.6 0.75]
                        [0.7 0.1 0.25])))))

(deftest test-sub-color
  (testing "subtracting colors"
    (is (eps3= [0.2 0.5 0.5]
                (sub [0.9 0.6 0.75]
                     [0.7 0.1 0.25])))))

(deftest test-scale-color
  (testing "scaling colors"
    (is (eps3= [0.4, 0.6, 0.8]
                (scale [0.2 0.3 0.4] 2)))))

(deftest test-mul-color
  (testing "multiplying colors"
    (is (eps3= [0.9 0.2 0.04]
                (mul [1 0.2 0.4] [0.9 1 0.1])))))
