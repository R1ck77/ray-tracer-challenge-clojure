(ns raytracer.materials-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.color :as color]
            [raytracer.materials :as materials]))

(deftest test-material
  (let [default-material (materials/material)]
    (testing "the default material"
      (is (c= (color/color 1 1 1) (:color default-material)))
      (is (= 0.1 (:ambient default-material)))
      (is (= 0.9 (:diffuse default-material)))
      (is (= 0.9 (:specular default-material)))
      (is (= 200.0 (:shiness default-material)))
      (is (= 0.0 (:reflectivity default-material)))
      (is (= 0.0 (:transparency default-material)))
      (is (= 1.0 (:refractive-index default-material))))
    (testing "material with custom color"
      (let [material (materials/material :color (color/color 1 2 3))]
        (is (c= (color/color 1 2 3) (:color material)))
        (is (= 0.1 (:ambient material)))
        (is (= 0.9 (:diffuse material)))
        (is (= 0.9 (:specular material)))
        (is (= 200.0 (:shiness material)))))
    (testing "material with custom ambient component"
      (let [material (materials/material :ambient 0.3)]
        (is (c= (color/color 1 1 1) (:color material)))
        (is (= 0.3 (:ambient material)))
        (is (= 0.9 (:diffuse material)))
        (is (= 0.9 (:specular material)))
        (is (= 200.0 (:shiness material)))))
    (testing "material with custom diffuse component"
      (let [material (materials/material :diffuse 0.7)]
        (is (c= (color/color 1 1 1) (:color material)))
        (is (= 0.1 (:ambient material)))
        (is (= 0.7 (:diffuse material)))
        (is (= 0.9 (:specular material)))
        (is (= 200.0 (:shiness material)))))
    (testing "material with custom specular component"
      (let [material (materials/material :specular 0.3)]
        (is (c= (color/color 1 1 1) (:color material)))
        (is (= 0.1 (:ambient material)))
        (is (= 0.9 (:diffuse material)))
        (is (= 0.3 (:specular material)))
        (is (= 200.0 (:shiness material)))))
    (testing "material with custom shiness"
      (let [material (materials/material :shiness 70)]
        (is (c= (color/color 1 1 1) (:color material)))
        (is (= 0.1 (:ambient material)))
        (is (= 0.9 (:diffuse material)))
        (is (= 0.9 (:specular material)))
        (is (= 70 (:shiness material)))))))

(deftest test-void-material
  (testing "the void material is black"
    (is (c= (color/color 0 0 0) (:color materials/void-material))))
  (testing "the void material has full transparency"
    (is (eps= 1.0 (:transparency materials/void-material))))
  (testing "the void material has 1 as refractive index"
    (is (eps= 1.0 (:refractive-index materials/void-material)))))

(deftest test-update-material
  (testing "a material can be updated"
    (is (= 14 (:ambient (materials/update-material (materials/material)
                                                   :ambient 14))))))
