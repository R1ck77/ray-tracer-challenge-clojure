(ns raytracer.shapes.smooth-triangle-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :as tu]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.ray :as ray]
            [raytracer.intersection :as intersection]
            [raytracer.shapes.shared :as shared]
            [raytracer.shapes.smooth-triangle :as st]))

(deftest test-smooth-triangle
  (let [tri (st/smooth-triangle (point/point -1 0 0)
                                (point/point 1 0 0)
                                (point/point 0 1 0)
                                (svector/svector 0 1 0)
                                (svector/svector -1 0 0)
                                (svector/svector 1 0 0))]
    (testing "Constructing a smooth triangle"
      (is (tu/t= (point/point -1 0 0) (:p1 tri)))
      (is (tu/t= (point/point 1 0 0) (:p2 tri)))
      (is (tu/t= (point/point 0 1 0) (:p3 tri)))
      (is (tu/t= (svector/svector 0 1 0) (:n1 tri)))
      (is (tu/t= (svector/svector -1 0 0) (:n2 tri)))
      (is (tu/t= (svector/svector 1 0 0) (:n3 tri))))))

(deftest test-local-intersect
  (let [tri (st/smooth-triangle (point/point 0 1 0)
                                (point/point -1 0 0)
                                (point/point 1 0 0)
                                (svector/svector 0 1 0)
                                (svector/svector -1 0 0)
                                (svector/svector 1 0 0))]
    (testing "An intersection with a smooth trinagle stores u/v"
      (let [ray (ray/ray (point/point -0.2 0.3 -2)
                         (svector/svector 0 0 1))]
        (is (tu/v= [0.45 0.25]
                   (intersection/getUV (first (shared/local-intersect tri ray)))))))
    (testing "A smooth triangle uses u/v to interpolate the normal"
      (let [intersection (intersection/uv-intersection 1 tri 0.45 0.25)]
        (is (tu/t= (svector/svector -0.5547 0.83205 0)
                   (shared/compute-normal tri (point/point 0 0 0) intersection)))))))
