(ns raytracer.shapes-test
  (:require [clojure.test :refer :all]
            [raytracer.shapes :as shapes]))

(deftest test-sphere
  (testing "sphere creates a shape with a radius"
   (is (:radius (shapes/sphere)))))

