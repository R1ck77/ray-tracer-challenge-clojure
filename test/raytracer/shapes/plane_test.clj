(ns raytracer.shapes.plane-test
  (:require [clojure.test :refer :all]
            [raytracer.const :as const]
            [raytracer.test-utils :refer :all]
            [raytracer.svector :as svector]
            [raytracer.shapes :as shapes]
            [raytracer.shapes.shared :as shared]
            [raytracer.point :as point]
            [raytracer.matrix :as matrix]
            [raytracer.transform :as transform]
            [raytracer.ray :as ray]
            [raytracer.shapes.plane :as plane]
            [raytracer.shapes.bounding-box :as bounding-box]
            [raytracer.shapes.placement :as placement]))

(def a-plane (plane/plane))

(deftest test-bounding-box
  (testing "The plane is its own bounding box"
    (is (= [(point/point const/neg-inf 0.0 const/neg-inf)
            (point/point const/inf 0.0 const/inf)]
           (bounding-box/get-corners (plane/plane))))))

(deftest test-constructor
  (testing "The courtesy constructor function fills material, inverse transform and its transpose"
    (is (:material (plane/plane)))
    (is (v= matrix/identity-matrix
           (-> (plane/plane) :placement placement/get-inverse-transform)))
    (is (v= matrix/identity-matrix
            (-> (plane/plane) :placement placement/get-inverse-transposed-transform)))))

(deftest test-plane-normal
  (let [expected-normal (svector/svector 0 1 0)]
    (testing "The normal of a plane is constant everywhere"
      (is (t= expected-normal
              (shared/compute-normal a-plane (point/point 0 0 0))))
      (is (t= expected-normal
              (shared/compute-normal a-plane (point/point 10 0 -10))))
      (is (t= expected-normal
              (shared/compute-normal a-plane (point/point 5 0 150)))))
    (testing "The normal of the plane is computed accounting for the transform"
      (let [transformed-plane (shared/change-transform a-plane
                                                (transform/rotate-x (/ Math/PI 2)))]
        (is (t= (svector/svector 0 0 1)
                (shared/compute-normal transformed-plane
                                       (point/point 1000 1000 0))))))))

(deftest test-ray-plane-intersect
  (testing "Intersect with a ray parallel to the plane"
    (is (empty? (ray/intersect (ray/ray (point/point 0 10 0)
                                        (svector/svector 0 0 1))
                               a-plane))))
  (testing "Intersect with a coplanar ray"
    (is (empty? (ray/intersect (ray/ray (point/point 0 0 0)
                                        (svector/svector 0 0 1))
                               a-plane))))
  (testing "A ray intersecting a plane from above"
    (let [intersections (ray/intersect (ray/ray (point/point 0 1 0)
                                                (svector/svector 0 -1 0))
                                       a-plane)]
      (is (= 1 (count intersections)))
      (is (eps= 1 (:t (first intersections))))
      (is (= a-plane (:object (first intersections))))))
  (testing "A ray intersecting a plane from below"
    (let [intersections (ray/intersect (ray/ray (point/point 0 -1 0)
                                                (svector/svector 0 1 0))
                                       a-plane)]
      (is (= 1 (count intersections)))
      (is (eps= 1 (:t (first intersections))))
      (is (= a-plane (:object (first intersections)))))))

(deftest test-includes?
  (let [shape (plane/plane)]
    (testing "The shape includes itself"
      (is (shared/includes? shape shape)))
    (testing "The shape does not include a copy of itself"
      (is (not (identical? (plane/plane) (plane/plane))))
      (is (not (shared/includes? shape (plane/plane)))))
    (testing "The shape does not include a different object"
      (is (not (shared/includes? shape (shapes/cube)))))))
