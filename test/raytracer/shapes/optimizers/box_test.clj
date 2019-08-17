(ns raytracer.shapes.optimizers.box-test
  (:require [clojure.test :refer :all]
            [raytracer.point :as point]
            [raytracer.shapes.optimizers.box :as box]))

(deftest test-constructor
  (testing "a box can be created from two points"
    (is (= (box/->Box (point/point -1 -2 -3)
                      (point/point 1 2 3))
           (box/box (point/point -1 -2 -3) (point/point 1 2 3)))))
  (testing "degenerate boxes are acceptable"
    (is (box/box (point/point 1 -2 -3) (point/point 1 2 3)))
    (is (box/box (point/point -1 2 -3) (point/point 1 2 3)))
    (is (box/box (point/point -1 -2 3) (point/point 1 2 3)))
    (is (box/box (point/point 1 2 3) (point/point 1 2 3))))
  (testing "coordinates must be ordered loosely ordered"
    (is (thrown? AssertionError (box/box (point/point 1 -2 -3) (point/point -1 2 3))))
    (is (thrown? AssertionError (box/box (point/point -1 2 -3) (point/point 1 -2 3))))
    (is (thrown? AssertionError (box/box (point/point -1 -2 3) (point/point 1 2 -3))))
    (is (thrown? AssertionError (box/box (point/point 1 2 3) (point/point -1 -2 -3))))))
