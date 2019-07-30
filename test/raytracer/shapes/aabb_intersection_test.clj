(ns raytracer.shapes.aabb-intersection-test
  (:require [clojure.test :refer :all]
            [raytracer.const :as const]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.ray :as ray]
            [raytracer.shapes.aabb-intersection :as aabb]))

(deftest test-hit-positive-centered-side-2
  (let [extremes [(point/point -1 -1 -1) (point/point 1 1 1)]]
    (testing "Hit on the X axis"
      (is (aabb/hit extremes (ray/ray (point/point 100 0 0)
                                      (svector/svector -1 0 0))))
      (is (aabb/hit extremes (ray/ray (point/point -100 0 0)
                                      (svector/svector 1 0 0)))))
    (testing "Hit on the Y axis"
      (is (aabb/hit extremes (ray/ray (point/point 0 100 0)
                                      (svector/svector 0 -1 0))))
      (is (aabb/hit extremes (ray/ray (point/point 0 -100 0)
                                      (svector/svector 0 1 0)))))
    (testing "Hit on the Z axis"
      (is (aabb/hit extremes (ray/ray (point/point 0 0 100)
                                      (svector/svector 0 0 -1))))
      (is (aabb/hit extremes (ray/ray (point/point 0 0 -100)
                                      (svector/svector 0 0 1)))))
    (testing "Hit on a YZ diagonal"
      (is (aabb/hit extremes (ray/ray (point/point 0 100 100)
                                      (svector/svector 0 -1 -1)))))))

(deftest test-hit-negative-centered-side-2
  (let [extremes [(point/point -1 -1 -1) (point/point 1 1 1)]]
    (testing "Miss on the X axis"
      (is (not (aabb/hit extremes (ray/ray (point/point 100 1.1 0)
                                       (svector/svector -1 0 0)))))
      (is (not (aabb/hit extremes (ray/ray (point/point -100 0 -1.1)
                                       (svector/svector 1 0 0))))))
    (testing "Miss on the Y axis"
      (is (not (aabb/hit extremes (ray/ray (point/point -1.1 100 0)
                                       (svector/svector 0 -1 0)))))
      (is (not (aabb/hit extremes (ray/ray (point/point 0 -100 1.1)
                                       (svector/svector 0 1 0))))))
    (testing "Miss on the Z axis"
      (is (not (aabb/hit extremes (ray/ray (point/point -1.1 0 100)
                                       (svector/svector 0 0 -1)))))
      (is (not (aabb/hit extremes (ray/ray (point/point 0 1.1 -100)
                                           (svector/svector 0 0 1))))))
    (testing "Miss on a random diagonal"
      (is (not (aabb/hit extremes (ray/ray (point/point 0 100 100)
                                       (svector/svector 1 0 0))))))))

(deftest test-hit-displaced-box-different-sides
  (let [extremes [(point/point -100 -10 6) (point/point -50 2000 100)]]
    (testing "?"
      (is (not (aabb/hit extremes (ray/ray (point/point -101 -8 -1000)
                                           (svector/svector 0 0 1))))))))
