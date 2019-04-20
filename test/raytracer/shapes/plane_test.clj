(ns raytracer.shape.plane-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.svector :as svector]
            [raytracer.shapes :as shapes]
            [raytracer.point :as point]
            [raytracer.transform :as transform]
            [raytracer.shapes.plane :as plane]))

(deftest test-plane-normal
  (let [plane (plane/plane)
        expected-normal (svector/svector 0 1 0)]
    (testing "The normal of a plane is constant everywhere"
      (is (v= expected-normal
              ((:normal plane) (point/point 0 0 0))))
      (is (v= expected-normal
              ((:normal plane) (point/point 10 0 -10))))
      (is (v= expected-normal
              ((:normal plane) (point/point 5 0 150)))))))
