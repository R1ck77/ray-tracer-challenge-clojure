(ns raytracer.shapes.shared-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.shapes.shared :as shared]))

(deftest test-as-point
  (is (v= (point/point 1 2 3)
          (shared/as-point (svector/svector 1 2 3)))))

(deftest test-as-vector
  (is (v= (svector/svector 1 2 3)
          (shared/as-vector (point/point 1 2 3)))))
