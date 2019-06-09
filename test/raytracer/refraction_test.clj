(ns raytracer.refraction-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.const :refer :all]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.shapes :as shapes]
            [raytracer.transform :as transform]
            [raytracer.material :as material]
            [raytracer.ray :as ray]            
            [raytracer.refraction :as refraction]
            [raytracer.world :as world]            
            [raytracer.intersection :as intersection]
            [raytracer.shapes-test :as shapes-test]))

(deftest test-compute-refractive-indices
    (testing "Finding n1 and n2 at various intersections"
    (let [sphere-a (-> (shapes-test/glass-sphere)
                       (shapes/change-transform (transform/scale 2 2 2))
                       (shapes/update-material #(material/update-material % :refractive-index 1.5)))
          sphere-b (-> (shapes-test/glass-sphere)
                       (shapes/change-transform (transform/translate 0 0 -0.25))
                       (shapes/update-material #(material/update-material % :refractive-index 2.0)))
          sphere-c (-> (shapes-test/glass-sphere)
                       (shapes/change-transform (transform/translate 0 0 0.25))
                       (shapes/update-material #(material/update-material % :refractive-index 2.5)))
          ray (ray/ray (point/point 0 0 -4)
                       (svector/svector 0 0 1))
          intersections (vec (map #(apply intersection/intersection %) [[2 sphere-a]
                                                                        [2.75 sphere-b]
                                                                        [3.25 sphere-c]
                                                                        [4.75 sphere-b]
                                                                        [5.25 sphere-c]
                                                                        [6 sphere-a]]))]
      (let  [indices (refraction/compute-refractive-indices (nth intersections 0) intersections 1)]
        (is (eps= 1 (:n1 indices)))
        (is (eps= 1.5 (:n2 indices))))
      (let  [indices (refraction/compute-refractive-indices (nth intersections 1) intersections 1)]
        (is (eps= 1.5 (:n1 indices)))
        (is (eps= 2.0 (:n2 indices))))
      (let  [indices (refraction/compute-refractive-indices (nth intersections 2) intersections 1)]
        (is (eps= 2 (:n1 indices)))
        (is (eps= 2.5 (:n2 indices))))
      (let  [indices (refraction/compute-refractive-indices (nth intersections 3) intersections 1)]
        (is (eps= 2.5 (:n1 indices)))
        (is (eps= 2.5 (:n2 indices))))
      (let  [indices (refraction/compute-refractive-indices (nth intersections 4) intersections 1)]
        (is (eps= 2.5 (:n1 indices)))
        (is (eps= 1.5 (:n2 indices))))
      (let  [indices (refraction/compute-refractive-indices  (nth intersections 5) intersections 1)]
        (is (eps= 1.5 (:n1 indices)))
        (is (eps= 1.0 (:n2 indices)))))))

(deftest test-schlick
  (testing "The Schlick approximation under total internal reflection"
    (let [shape (shapes-test/glass-sphere)
          ray (ray/ray (point/point 0 0 half√2)
                       (svector/svector 0 1 0))
          intersections [(intersection/intersection (- half√2) shape)
                         (intersection/intersection half√2 shape)]
          intermediate-results (world/prepare-computations ray
                                                           (second intersections)
                                                           {:n1 1.5, :n2 1})]
      (is (eps= 1 (refraction/schlick intermediate-results)))))
  (testing "The Schlick approximation with a perpendicular viewing angle"
    (let [shape (shapes-test/glass-sphere)
          ray (ray/ray (point/point 0 0 0)
                       (svector/svector 0 1 0))
          intersections [(intersection/intersection -1 shape)
                         (intersection/intersection 1 shape)]
          intermediate-results (world/prepare-computations ray
                                                           (second intersections)
                                                           {:n1 1.5, :n2 1})]
      (is (eps= 0.04 (refraction/schlick intermediate-results)))))
  (testing "The Schlick approximation with small angle and n2 > n1"
    (let [shape (shapes-test/glass-sphere)
          ray (ray/ray (point/point 0 0.99 -2)
                       (svector/svector 0 0 1))
          intersections [(intersection/intersection 1.8589 shape)]
          intermediate-results (world/prepare-computations ray
                                                           (first intersections)
                                                           {:n1 1, :n2 1.5})]
      (is (eps= 0.48873 (refraction/schlick intermediate-results))))))
