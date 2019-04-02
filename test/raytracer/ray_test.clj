(ns raytracer.ray-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.ray :as ray]))

(deftest test-ray
  (testing "ray creation"
    (let [origin (point/point 1 2 3)
          direction (svector/svector 4 5 6)
          ray (ray/create origin direction)]
      (is (identical? origin (:origin ray)))
      (is (identical? direction (:direction ray))))))

(deftest test-position
  (let [ray (ray/create (point/point 2 3 4)
                        (svector/svector 1 0 0))]
  (testing "t=0"
    (is (eps4= (point/point 2 3 4)
               (ray/position ray 0))))
  (testing "t=1"
    (is (eps4= (point/point 3 3 4)
               (ray/position ray 1))))
  (testing "t=-1"
    (is (eps4= (point/point 1 3 4)
               (ray/position ray -1))))
  (testing "t=2.5"
    (is (eps4= (point/point 4.5 3 4)
               (ray/position ray 2.5))))))

(deftest test-sphere-intersection
  (testing "a ray intersects a sphere at two points"
    (let [intersection (ray/intersect (ray/create (point/point 0 0 -5)
                                                  (svector/svector 0 0 1))
                                      (ray/sphere))]
      (is (= 2 (:count intersection)))
      (is (eps2= [4 6] (:values intersection)))))
  (testing "a ray intersects a sphere at a tangent"
    (let [intersection (ray/intersect (ray/create (point/point 0 1 -5)
                                                  (svector/svector 0 0 1))
                                      (ray/sphere))]
      (is (= 2 (:count intersection)))
      (is (eps2= [5 5] (:values intersection)))))
  (testing "a ray misses a sphere"
    (let [intersection (ray/intersect (ray/create (point/point 0 2 -5)
                                                  (svector/svector 0 0 1))
                                      (ray/sphere))]
      (is (= 0 (:count intersection)))))
  (testing "a ray originates inside a sphere"
    (let [intersection (ray/intersect (ray/create (point/point 0 0 0)
                                                  (svector/svector 0 0 1))
                                      (ray/sphere))]
      (is (= 2 (:count intersection)))
      (is (eps2= [-1 1] (:values intersection)))))
  (testing "a sphere is behind a ray"
    (let [intersection (ray/intersect (ray/create (point/point 0 0 5)
                                                  (svector/svector 0 0 1))
                                      (ray/sphere))]
      (is (= 2 (:count intersection)))
      (is (eps2= [-6 -4] (:values intersection))))))
