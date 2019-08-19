(ns raytracer.shapes.smooth-triangle-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :as tu]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.shapes.smooth-triangle :as st]))

(def tri (st/smooth-triangle (point/point -1 0 0)
                             (point/point 1 0 0)
                             (point/point 0 1 0)
                             (svector/svector 0 1 0)
                             (svector/svector -1 0 0)
                             (svector/svector 1 0 0)))

(deftest test-smooth-triangle
  (testing "Constructing a smooth triangle"
    (is (tu/t= (point/point -1 0 0) (:p1 tri)))
    (is (tu/t= (point/point 1 0 0) (:p2 tri)))
    (is (tu/t= (point/point 0 1 0) (:p3 tri)))
    (is (tu/t= (svector/svector 0 1 0) (:n1 tri)))
    (is (tu/t= (svector/svector -1 0 0) (:n2 tri)))
    (is (tu/t= (svector/svector 1 0 0) (:n3 tri)))))
