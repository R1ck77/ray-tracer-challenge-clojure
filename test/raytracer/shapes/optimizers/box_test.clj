(ns raytracer.shapes.optimizers.box-test
  (:require [clojure.test :refer :all]
            [raytracer.point :as point]
            [raytracer.shapes :as shapes]
            [raytracer.shapes.optimizers.box :as box]))

(deftest test-constructor
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
    (is (box/contains-shape? (box/box (point/point -1 -1 -1)
                                      (point/point 1 1 1))
                             (shapes/sphere)))
    (is (box/contains-shape? (box/box (point/point -1.5 -1.1 -1.3)
                               (point/point 1.3 1.2 1.1))
                      (shapes/sphere))))
  (testing "negative cases"
    (is (not (box/contains-shape? (box/box (point/point -0.999 -1 -1)
                                    (point/point 1 1 1))
                           (shapes/sphere))))

    (is (not (box/contains-shape? (box/box (point/point -1 -0.999 -1)
                                    (point/point 1 1 1))
                           (shapes/sphere))))
    (is (not (box/contains-shape? (box/box (point/point -1 -1 -0.999)
                                    (point/point 1 1 1))
                           (shapes/sphere))))
    (is (not (box/contains-shape? (box/box (point/point -1 -1 -1)
                                    (point/point 0.999 1 1))
                           (shapes/sphere))))
    (is (not (box/contains-shape? (box/box (point/point -1 -1 -1)
                                    (point/point 1 0.999 1))
                           (shapes/sphere))))
    (is (not (box/contains-shape? (box/box (point/point -1 -1 -1)
                                    (point/point 1 1 0.999))
                           (shapes/sphere))))))

(deftest test-partition
  (testing "partition on generic boxes returns a set with 8 new boxes"
    (let [result (box/bisect (box/box (point/point 0 0 0)
                                         (point/point 1 1 1)))]
      (is (= 8 (count result)))
      (is (set? result))))
  (testing "partition works on degenerate boxes, but returns the original box without changes"
    (is (= 1 (count (box/bisect (box/box (point/point 0 0 0)
                                         (point/point 0 0 0))))))
    (is (= 2 (count (box/bisect (box/box (point/point 0 2 0)
                                         (point/point 1 2 0))))))
    (is (= 4 (count (box/bisect (box/box (point/point 0 0 0)
                                         (point/point 1 2 0))))))))
