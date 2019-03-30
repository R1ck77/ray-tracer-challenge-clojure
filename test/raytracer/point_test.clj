(ns raytracer.point-test
  (:require [clojure.test :refer :all]
            [raytracer.point :refer :all]
            [raytracer.svector :refer [svector?]]
            [raytracer.test-utils :refer [eps]]))

(deftest test-point?
  (testing "point? returns true for tuple when w is 1"
    (is (point? [3.5 1.2 5.4 1.0]))
    (is (point? [32 12.0 -19.0 (+ 1 (/ eps 2))]))
    (is (point? [3.5 1.2 5.4 (- 1 (/ eps 2))])))
  (testing "point? returns false for tuple with w not 1"
    (is (not (point? [3.5 1.2 5.4 0])))
    (is (not (point? [32 12.0 -19.0 (+ 1 0.002)])))
    (is (not (point? [3.5 1.2 5.4 (- 1 0.002)])))))

(deftest test-point
  (testing "point creates a tuple with w == 1"
    (is (point? (point 1 2 3)))
    (is (not (svector? (point 4 1 2))))))


