(ns raytracer.phong-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.phong :as phong]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.shapes :as shapes]
            [raytracer.materials :as materials]
            [raytracer.pattern :as pattern]
            [raytracer.light-sources :as light-sources]))

(def half√2 (/ (Math/sqrt 2) 2))
(def material (materials/material))
(def position (point/point 0 0 0))

(defn test-object [material]
  (shapes/change-material (shapes/sphere) material))

(deftest test-lighting
  (testing "Lighting with the eye between the light and the surface"
    (let [eye (svector/svector 0 0 -1)
          normal (svector/svector 0 0 -1)
          light (light-sources/create-point-light (point/point 0 0 -10) [1 1 1])]
      (is (v= [1.9 1.9 1.9]
              (phong/lighting (test-object material) light position eye normal)))))
  (testing "Lighting with the surface in shadow"
    (let [eye (svector/svector 0 0 -1)
          normal (svector/svector 0 0 -1)
          light (light-sources/create-point-light (point/point 0 0 -10) [1 1 1])]
      (is (v= [0.1 0.1 0.1]
              (phong/lighting (test-object material) light position eye normal 0.0)))))
  (testing "Lighting with the eye between light and surface eye offset 45°"
    (let [eye (svector/svector 0 half√2 (- half√2))
          normal (svector/svector 0 0 -1)
          light (light-sources/create-point-light (point/point 0 0 -10) [1 1 1])]
      (is (v= [1.0 1.0 1.0]
              (phong/lighting (test-object material) light position eye normal)))))
  (testing "Lighting with eye opposite surface, light offset 45°"
    (let [eye (svector/svector 0 0 -1)
          normal (svector/svector 0 0 -1)
          light (light-sources/create-point-light (point/point 0 10 -10) [1 1 1])]
      (is (v= [0.7364 0.7364 0.7364]
              (phong/lighting (test-object material) light position eye normal)))))
  (testing "Lighting with eye in the path of the reflection vector"
    (let [eye (svector/svector 0 (- half√2) (- half√2))
          normal (svector/svector 0 0 -1)
          light (light-sources/create-point-light (point/point 0 10 -10) [1 1 1])]
      (is (v= [1.6364 1.6364 1.6364]
              (phong/lighting (test-object material) light position eye normal)))))
  (testing "Lighting with eye in the path of the reflection vector and partial shadowing (golden master)"
    (let [eye (svector/svector 0 (- half√2) (- half√2))
          normal (svector/svector 0 0 -1)
          light (light-sources/create-point-light (point/point 0 10 -10) [1 1 1])]
      (is (v= [1.06793 1.06793 1.06793]
              (phong/lighting (test-object material) light position eye normal 0.63)))))  
  (testing "Lighting with the light behind the surface"
    (let [eye (svector/svector 0 0 -1)
      normal (svector/svector 0 0 -1)
      light (light-sources/create-point-light (point/point 0 0 10) [1 1 1])]
      (is (v= [0.1 0.1 0.1]
              (phong/lighting (test-object material) light position eye normal)))))
  (testing "Lighting with a pattern applied"
    (let [material (materials/material :ambient 1, :diffuse 0, :specular 0,
                                       :pattern (pattern/stripe [1 1 1] [0 0 0]))
          eye (svector/svector 0 0 -1)
          normal (svector/svector 0 0 -1)
          light (light-sources/create-point-light (point/point 0 0 -10)
                                                  [1 1 1])]
      (is (v= [1 1 1]
              (phong/lighting (test-object material) light (point/point 0.9 0 0) eye normal 1.0)))
      (is (v= [0 0 0]
              (phong/lighting (test-object material) light (point/point 1.1 0 0) eye normal 1.0)))))
  (testing "Lighting with a pattern applied: golden master with partial shadowing"
    (let [material (materials/material :ambient 1, :diffuse 0, :specular 0,
                                       :pattern (pattern/stripe [1 1 1] [0 0 0]))
          eye (svector/svector 0 0 -1)
          normal (svector/svector 0 0 -1)
          light (light-sources/create-point-light (point/point 0 0 -10)
                                                  [1 1 1])]
      (is (v= [1 1 1]
              (phong/lighting (test-object material) light (point/point 0.9 0 0) eye normal 1.0)))
      (is (v= [0 0 0]
              (phong/lighting (test-object material) light (point/point 1.1 0 0) eye normal 1.0))))))
