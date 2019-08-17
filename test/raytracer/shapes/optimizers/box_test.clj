(ns raytracer.shapes.optimizers.box-test
  (:require [clojure.test :refer :all]
            [raytracer.point :as point]
            [raytracer.shapes :as shapes]
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

(deftest test-contains
  (testing "positive cases"
    (is (box/contains (box/box (point/point -1 -1 -1)
                               (point/point 1 1 1))
                      (shapes/sphere)))
    (is (box/contains (box/box (point/point -1.5 -1.1 -1.3)
                               (point/point 1.3 1.2 1.1))
                      (shapes/sphere))))
  (testing "negative cases"
    (is (not (box/contains (box/box (point/point -0.999 -1 -1)
                                    (point/point 1 1 1))
                           (shapes/sphere))))

    (is (not (box/contains (box/box (point/point -1 -0.999 -1)
                                    (point/point 1 1 1))
                           (shapes/sphere))))
    (is (not (box/contains (box/box (point/point -1 -1 -0.999)
                                    (point/point 1 1 1))
                           (shapes/sphere))))
    (is (not (box/contains (box/box (point/point -1 -1 -1)
                                    (point/point 0.999 1 1))
                           (shapes/sphere))))
    (is (not (box/contains (box/box (point/point -1 -1 -1)
                                    (point/point 1 0.999 1))
                           (shapes/sphere))))
    (is (not (box/contains (box/box (point/point -1 -1 -1)
                                    (point/point 1 1 0.999))
                           (shapes/sphere))))))
