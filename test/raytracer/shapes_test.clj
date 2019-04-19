(ns raytracer.shapes-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]
            [raytracer.shapes :as shapes]
            [raytracer.intersection :as intersection]
            [raytracer.ray :as ray]
            [raytracer.transform :as transform]
            [raytracer.materials :as materials]))

(deftest test-as-point
  (is (v= (point/point 1 2 3)
          (shapes/as-point (svector/svector 1 2 3)))))

(deftest test-as-vector
  (is (v= (svector/svector 1 2 3)
          (shapes/as-vector (point/point 1 2 3)))))
