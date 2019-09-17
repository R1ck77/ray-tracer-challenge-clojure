(ns raytracer.shapes.triangle-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :as tu]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.shapes :as shapes]
            [raytracer.shapes.shared :as shared]
            [raytracer.shapes.triangle :as triangle]
            [raytracer.ray :as ray]
            [raytracer.grouping.hierarchy :as hierarchy]))

(deftest test-constructor
  (let [triangle (triangle/triangle (point/point 0 1 0)
                                    (point/point -1 0 0)
                                    (point/point 1 0 0))]
    
    (testing "A triangle has 3 vertices"
      (is (tu/t= (point/point 0  1 0) (:p1 triangle)))
      (is (tu/t= (point/point -1 0 0) (:p2 triangle)))
      (is (tu/t= (point/point 1  0 0) (:p3 triangle))))
    (testing "A triangle has 2 computed edges"
      (is (tu/t= (svector/svector -1 -1 0) (:e1 triangle)))
      (is (tu/t= (svector/svector 1 -1 0 ) (:e2 triangle))))
    (testing "A triangle has a normal"
      (is (tu/t= (svector/svector 0.0 0.0 -1.0) (:normal triangle))))))

(deftest test-compute-normal
  (testing "Finding the normal on a triangle"
    (let [triangle (triangle/triangle (point/point 0 1 0)
                                      (point/point -1 0 0)
                                      (point/point 1 0 0))
          normal (:normal triangle)]
      (is (tu/t= normal (shared/compute-normal triangle
                                               (point/point 0 0.5 0)
                                               (hierarchy/hierarchy triangle))))
      (is (tu/t= normal (shared/compute-normal triangle
                                               (point/point -0.5 0.75 0)
                                               (hierarchy/hierarchy triangle))))
      (is (tu/t= normal (shared/compute-normal triangle
                                               (point/point 0.5 0.25 0)
                                               (hierarchy/hierarchy triangle)))))))

(deftest test-local-intersect
  (let [triangle (triangle/triangle (point/point 0 1 0)
                                    (point/point -1 0 0)
                                    (point/point 1 0 0))]
    (testing "Intersecting a ray parallel to the triangle"
      (is (= []
             (shared/local-intersect triangle
                                     (ray/ray (point/point 0 -1 -2)
                                              (svector/svector 0 1 0))))))
    (testing "A ray misses the p1-p3 edge"
      (is (= []
             (shared/local-intersect triangle
                                     (ray/ray (point/point 1 1 -2)
                                              (svector/svector 0 0 1))))))
    (testing "A ray misses the p1-p2 edge"
      (is (= []
             (shared/local-intersect triangle
                                     (ray/ray (point/point -1 1 -2)
                                              (svector/svector 0 0 1))))))
    (testing "A ray misses the p2-p3 edge"
      (is (= []
             (shared/local-intersect triangle
                                     (ray/ray (point/point 0 -1 -2)
                                              (svector/svector 0 0 1))))))
    (testing "A ray strikes a triangle"
      (is (tu/v= [2]
                 (map :t (shared/local-intersect triangle
                                                 (ray/ray (point/point 0 0.5 -2)
                                                          (svector/svector 0 0 1)))))))))

(deftest test-includes?
  (let [p1 (point/point 0 1 2)
        p2 (point/point 3 4 5)
        p3 (point/point 0 0 0)
        shape (triangle/triangle p1 p2 p3)]
    (testing "The shape includes itself"
      (is (shared/includes? shape shape)))
    (testing "The shape does not include a copy of itself"
      (is (not (identical? (triangle/triangle p1 p2 p3)
                           (triangle/triangle  p1 p2 p3))))
      (is (not (shared/includes? shape (triangle/triangle p1 p2 p3)))))
    (testing "The shape does not include a different object"
      (is (not (shared/includes? shape (shapes/cube)))))))
