(ns raytracer.shapes.triangle-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :as tu]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.shapes.triangle :as triangle]))

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
