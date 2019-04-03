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
    (is (v= (point/point 2 3 4)
               (ray/position ray 0))))
  (testing "t=1"
    (is (v= (point/point 3 3 4)
               (ray/position ray 1))))
  (testing "t=-1"
    (is (v= (point/point 1 3 4)
               (ray/position ray -1))))
  (testing "t=2.5"
    (is (v= (point/point 4.5 3 4)
               (ray/position ray 2.5))))))

(deftest test-sphere-intersection
  (let [sphere (ray/sphere)]
    (testing "a ray intersects a sphere at two points"
      (let [intersections (ray/intersect (ray/create (point/point 0 0 -5)
                                                     (svector/svector 0 0 1))
                                         sphere)]
        (is (= 2 (:count intersections)))
        (is (identical? sphere (:object (first (:values intersections)))))
        (is (identical? sphere (:object (second (:values intersections)))))
        (is (eps= 4 (:t (first (:values intersections)))))
        (is (eps= 6 (:t (second (:values intersections)))))))
    (testing "a ray intersects a sphere at a tangent"
      (let [intersections (ray/intersect (ray/create (point/point 0 1 -5)
                                                     (svector/svector 0 0 1))
                                         sphere)]
        (is (= 2 (:count intersections)))
        (is (identical? sphere (:object (first (:values intersections)))))
        (is (identical? sphere (:object (second (:values intersections)))))
        (is (eps= 5 (:t (first (:values intersections)))))
        (is (eps= 5 (:t (second (:values intersections)))))))
    (testing "a ray misses a sphere"
      (let [intersections (ray/intersect (ray/create (point/point 0 2 -5)
                                                     (svector/svector 0 0 1))
                                         sphere)]
        (is (= 0 (:count intersections)))))
    (testing "a ray originates inside a sphere"
      (let [intersections (ray/intersect (ray/create (point/point 0 0 0)
                                                     (svector/svector 0 0 1))
                                         sphere)]
        (is (= 2 (:count intersections)))
        (is (identical? sphere (:object (first (:values intersections)))))
        (is (identical? sphere (:object (second (:values intersections)))))
        (is (eps= -1 (:t (first (:values intersections)))))
        (is (eps= 1 (:t (second (:values intersections)))))))
    (testing "a sphere is behind a ray"
      (let [intersections (ray/intersect (ray/create (point/point 0 0 5)
                                                     (svector/svector 0 0 1))
                                         sphere)]
        (is (= 2 (:count intersections)))
        (is (identical? sphere (:object (first (:values intersections)))))
        (is (identical? sphere (:object (second (:values intersections)))))
        (is (eps= -6 (:t (first (:values intersections)))))
        (is (eps= -4 (:t (second (:values intersections)))))))))

(deftest test-intersection
  (testing "create new intersection"
    (let [sphere (ray/sphere)
          intersection (ray/intersection 2.4 sphere)]
      (is (identical? sphere (:object intersection)))
      (is (= 2.4 (:t intersection))))))

(deftest test-intersections
  (testing "exciting \"intersections\" function"
      (let [i1 (ray/intersection 2.4 (ray/sphere))
             i2 (ray/intersection 2.8 (ray/sphere))
            intersections (ray/intersections i1 i2)]
        (is (= 2 (count intersections)))
        (is (identical? i1 (first intersections)))
        (is (identical? i2 (second intersections))))))
