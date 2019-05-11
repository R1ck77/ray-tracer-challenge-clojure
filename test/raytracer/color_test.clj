(ns raytracer.color-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.color :as color]))

(deftest test-add-color
  (testing "adding color"
    (is (c= (color/color 1.6 0.7 1.0)
            (color/add (color/color 0.9 0.6 0.75)
                       (color/color 0.7 0.1 0.25))))))

(deftest test-sub-color
  (testing "subtracting color"
    (is (c= (color/color 0.2 0.5 0.5)
            (color/sub (color/color 0.9 0.6 0.75)
                       (color/color 0.7 0.1 0.25))))))

(deftest test-scale-color
  (testing "scaling color"
    (is (c= (color/color 0.4, 0.6, 0.8)
            (color/scale (color/color 0.2 0.3 0.4) 2)))))

(deftest test-mul-color
  (testing "multiplying color"
    (is (c= (color/color 0.9 0.2 0.04)
            (color/mul (color/color 1 0.2 0.4)
                       (color/color 0.9 1 0.1))))))
