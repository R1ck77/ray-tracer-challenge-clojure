(ns raytracer.material-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.color :as color]
            [raytracer.material :as material]))

(deftest test-material
  (let [default-material (material/material)]
    (testing "the default material"
      (is (c= (color/color 1 1 1) (material/get-color default-material)))
      (is (= 0.1 (:ambient default-material)))
      (is (= 0.9 (:diffuse default-material)))
      (is (= 0.9 (:specular default-material)))
      (is (= 200.0 (:shiness default-material)))
      (is (= 0.0 (:reflectivity default-material)))
      (is (= 0.0 (:transparency default-material)))
      (is (= 1.0 (:refractive-index default-material))))
    (testing "material with custom color"
      (let [material (material/material (color/color 1 2 3))]
        (is (c= (color/color 1 2 3) (:color material)))
        (is (= 0.1 (:ambient material)))
        (is (= 0.9 (:diffuse material)))
        (is (= 0.9 (:specular material)))
        (is (= 200.0 (:shiness material)))))
    (testing "material with custom ambient component"
      (let [material (material/material :ambient 0.3)]
        (is (c= (color/color 1 1 1) (:color material)))
        (is (= 0.3 (:ambient material)))
        (is (= 0.9 (:diffuse material)))
        (is (= 0.9 (:specular material)))
        (is (= 200.0 (:shiness material)))))
    (testing "material with custom diffuse component"
      (let [material (material/material :diffuse 0.7)]
        (is (c= (color/color 1 1 1) (:color material)))
        (is (= 0.1 (:ambient material)))
        (is (= 0.7 (:diffuse material)))
        (is (= 0.9 (:specular material)))
        (is (= 200.0 (:shiness material)))))
    (testing "material with custom specular component"
      (let [material (material/material :specular 0.3)]
        (is (c= (color/color 1 1 1) (:color material)))
        (is (= 0.1 (:ambient material)))
        (is (= 0.9 (:diffuse material)))
        (is (= 0.3 (:specular material)))
        (is (= 200.0 (:shiness material)))))
    (testing "material with custom shiness"
      (let [material (material/material :shiness 70)]
        (is (c= (color/color 1 1 1) (:color material)))
        (is (= 0.1 (:ambient material)))
        (is (= 0.9 (:diffuse material)))
        (is (= 0.9 (:specular material)))
        (is (= 70 (:shiness material)))))))

(deftest test-void-material
  (testing "the void material is black"
    (is (c= (color/color 0 0 0) (:color material/void-material))))
  (testing "the void material has full transparency"
    (is (eps= 1.0 (:transparency material/void-material))))
  (testing "the void material has 1 as refractive index"
    (is (eps= 1.0 (:refractive-index material/void-material)))))

(deftest test-update-material
  (testing "a material can be updated"
    (is (= 14 (:ambient (material/update-material (material/material)
                                                   :ambient 14))))))
