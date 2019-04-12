(ns raytracer.world-test
  (:require [clojure.test :refer :all]
            [raytracer.world :as world]
            [raytracer.point :as point]
            [raytracer.ray :as ray]
            [raytracer.materials :as materials]
            [raytracer.transform :as transform]
            [raytracer.light-sources :as light-sources]))

(deftest test-create
  (testing "Creating a world"
   (let [world (world/create)]
     (is (empty? (:objects world)))
     (is (empty? (:light-sources world))))))

(deftest test-default-world
  (testing "The default world"
    (let [world (world/default-world)
          expected-sphere1 (ray/change-transform (ray/sphere)
                                                 (transform/scale 0.5 0.5 0.5))
          expected-sphere2 (ray/change-material (ray/sphere)
                                                (materials/material :color [0.8 1.0 0.6]
                                                                    :diffuse 0.7
                                                                    :specular 0.2))]
      (is (contains? (:light-sources world)
                     (light-sources/create-point-light (point/point -10 10 -10)
                                                       [1 1 1])))
      (is (some #(ray/same-sphere? expected-sphere1 %) (:objects world)))
      (is (some #(ray/same-sphere? expected-sphere2 %) (:objects world))))))
