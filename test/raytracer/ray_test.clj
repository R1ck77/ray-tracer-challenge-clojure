(ns raytracer.ray-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]            
            [raytracer.ray :as ray]
            [raytracer.shapes :as shapes]
            [raytracer.transform :as transform]
            [raytracer.materials :as materials]))

(deftest test-ray
  (testing "ray creation"
    (let [origin (point/point 1 2 3)
          direction (svector/svector 4 5 6)
          ray (ray/ray origin direction)]
      (is (identical? origin (:origin ray)))
      (is (identical? direction (:direction ray))))))

(deftest test-position
  (let [ray (ray/ray (point/point 2 3 4)
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

(deftest test-transform
  (let [ray (ray/ray (point/point 1 2 3)
                     (svector/svector 0 1 0))]
    (testing "translating a ray"
      (let [result (ray/transform ray (transform/translate 3 4 5))]
        (is (v= [4 6 8 1] (:origin result)))
        (is (v= [0 1 0 0] (:direction result)))))
    (testing "scaling a ray"
      (let [result (ray/transform ray (transform/scale 2 3 4))]
        (is (v= [2 6 12 1] (:origin result)))
        (is (v= [0 3 0 0] (:direction result)))))))


