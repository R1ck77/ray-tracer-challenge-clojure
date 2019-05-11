(ns raytracer.shapes.plane-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.svector :as svector]
            [raytracer.shapes :as shapes]
            [raytracer.point :as point]
            [raytracer.transform :as transform]
            [raytracer.ray :as ray]
            [raytracer.shapes.plane :as plane]))

(def plane (plane/plane))

(deftest test-plane-normal
  (let [expected-normal (svector/svector 0 1 0)]
    (testing "The normal of a plane is constant everywhere"
      (is (t= expected-normal
              ((:normal plane) plane (point/point 0 0 0))))
      (is (t= expected-normal
              ((:normal plane) plane (point/point 10 0 -10))))
      (is (t= expected-normal
              ((:normal plane) plane (point/point 5 0 150)))))))

(deftest test-ray-plane-intersect
  (testing "Intersect with a ray parallel to the plane"
    (is (empty? (ray/intersect (ray/ray (point/point 0 10 0)
                                        (svector/svector 0 0 1))
                               plane))))
  (testing "Intersect with a coplanar ray"
    (is (empty? (ray/intersect (ray/ray (point/point 0 0 0)
                                        (svector/svector 0 0 1))
                               plane))))
  (testing "A ray intersecting a plane from above"
    (let [intersections (ray/intersect (ray/ray (point/point 0 1 0)
                                                (svector/svector 0 -1 0))
                                       plane)]
      (is (= 1 (count intersections)))
      (is (eps= 1 (:t (first intersections))))
      (is (= plane (:object (first intersections))))))
  (testing "A ray intersecting a plane from below"
    (let [intersections (ray/intersect (ray/ray (point/point 0 -1 0)
                                                (svector/svector 0 1 0))
                                       plane)]
      (is (= 1 (count intersections)))
      (is (eps= 1 (:t (first intersections))))
      (is (= plane (:object (first intersections)))))))
