(ns raytracer.shapes-test
  (:require [clojure.test :refer :all]
            [raytracer.material :as material]
            [raytracer.shapes :as shapes]
            [raytracer.shapes.sphere :as sphere-ns]
            [raytracer.shapes.plane :as plane-ns]
            [raytracer.shapes.cube :as cube-ns]
            [raytracer.shapes.cylinder :as cylinder-ns]
            [raytracer.shapes.cone :as cone-ns]
            [raytracer.shapes.group :as group-ns]))

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
    (is (= (plane-ns/plane) (shapes/plane))))
  (testing "cube creates a cube"
    (is (= (cube-ns/cube) (shapes/cube))))
  (testing "cylinder creates a cylinder"
    (is (= (cylinder-ns/cylinder) (shapes/cylinder)))
    (is (= true (:closed (shapes/cylinder :closed true)))))
  (testing "cone creates a cone"
    (is (= (cone-ns/cone) (shapes/cone)))
    (is (= true (:closed (shapes/cone :closed true)))))
  (testing "group creates a group"
    (is (= (group-ns/group []) (shapes/group)))
    (is (= (group-ns/group [(shapes/sphere)])
           (shapes/group (shapes/sphere))))))
