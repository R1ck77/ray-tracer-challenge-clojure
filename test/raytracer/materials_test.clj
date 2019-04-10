(ns raytracer.materials-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.materials :as materials]))

(deftest test-material
  (testing "the default material"
    (let [material (materials/material)]
      (is (= [1 1 1] (:color material)))
      (is (= 0.1 (:ambient material)))
      (is (= 0.9 (:diffuse material)))
      (is (= 0.9 (:specular material)))
      (is (= 200.0 (:shiness material))))))
