(ns raytracer.phong-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.phong :as phong]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.materials :as materials]
            [raytracer.light-sources :as light-sources]))

(def half√2 (/ (Math/sqrt 2) 2))
(def material (materials/material))
(def position (point/point 0 0 0))

(deftest test-lighting
  (testing "Lighting with the eye between the light and the surface"
    (let [eye (svector/svector 0 0 -1)
          normal (svector/svector 0 0 -1)
          light (light-sources/create-point-light (point/point 0 0 -10) [1 1 1])]
      (is (v= [1.9 1.9 1.9]
              (phong/lighting material light position eye normal)))))
  (testing "Lighting with the eye between light and surface eye offset 45°"
    (let [eye (svector/svector 0 half√2 (- half√2))
          normal (svector/svector 0 0 -1)
          light (light-sources/create-point-light (point/point 0 0 -10) [1 1 1])]
      (is (v= [1.0 1.0 1.0]
              (phong/lighting material light position eye normal)))))
  (testing "Lighting with eye opposite surface, light offset 45°"
    (let [eye (svector/svector 0 0 -1)
          normal (svector/svector 0 0 -1)
          light (light-sources/create-point-light (point/point 0 10 -10) [1 1 1])]
      (is (v= [0.7364 0.7364 0.7364]
              (phong/lighting material light position eye normal)))))
  (testing "Lighting with eye in the path of the reflection vector"
    (let [eye (svector/svector 0 (- half√2) (- half√2))
          normal (svector/svector 0 0 -1)
          light (light-sources/create-point-light (point/point 0 10 -10) [1 1 1])]
      (is (v= [1.6364 1.6364 1.6364]
              (phong/lighting material light position eye normal)))))
  (testing "Lighting with the light behind the surface"
    (let [eye (svector/svector 0 0 -1)
      normal (svector/svector 0 0 -1)
      light (light-sources/create-point-light (point/point 0 0 10) [1 1 1])]
      (is (v= [0.1 0.1 0.1]
              (phong/lighting material light position eye normal))))))
