(ns raytracer.shapes-test
  (:require [clojure.test :refer :all]
            [raytracer.materials :as materials]
            [raytracer.shapes :as shapes]))

(deftest test-sphere
  (testing "sphere creates a shape with a radius"
    (is (:radius (shapes/sphere)))))

(deftest test-glass-object
  (testing "A helper for producing a sphere with a glassy material"
    (let [glass-sphere-material (:material (shapes/glass-sphere))]
      (is (= 1.0 (:transparency glass-sphere-material)))
      (is (= 1.5 (:refractive-index glass-sphere-material))))))

(deftest test-update-maerial
  (testing "Can update a material using a specific function"
    (is (= 10 (:shiness (:material (shapes/update-material (shapes/glass-sphere)
                                                           #(materials/update-material % :shiness 10))))))))
