(ns raytracer.shapes-test
  (:require [clojure.test :refer :all]
            [raytracer.material :as material]
            [raytracer.shapes :as shapes]
            [raytracer.shapes.sphere :as sphere-ns]
            [raytracer.shapes.plane :as plane-ns]))

(defn- glass-object [shape]
  (shapes/change-material shape
                          (material/update-material (:material shape)
                                                    :transparency 1.0
                                                    :refractive-index 1.5)))

(defn glass-sphere []
  (glass-object (sphere-ns/sphere)))

(deftest test-glass-object
  (testing "A helper for producing a sphere with a glassy material"
    (let [glass-sphere-material (:material (glass-sphere))]
      (is (= 1.0 (:transparency glass-sphere-material)))
      (is (= 1.5 (:refractive-index glass-sphere-material))))))

(deftest test-update-maerial
  (testing "Can update a material using a specific function"
    (is (= 10 (:shiness (:material (shapes/update-material (glass-sphere)
                                                           #(material/update-material % :shiness 10))))))))

(deftest test-courtesy-shapes-functions
  (testing "sphere creates a sphere"
    (is (= (sphere-ns/sphere) (shapes/sphere))))
  (testing "plane creates a plane"
    (is (= (sphere-ns/plane) (shapes/plane))))
  (testing "cube creates a cube"
    (is (= (sphere-ns/cube) (shapes/cube)))))
