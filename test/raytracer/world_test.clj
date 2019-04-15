(ns raytracer.world-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.world :as world]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
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
          expected-sphere1 (ray/change-material (ray/sphere)
                                                (materials/material :color [0.8 1.0 0.6]
                                                                    :diffuse 0.7
                                                                    :specular 0.2))
          expected-sphere2 (ray/change-transform (ray/sphere)
                                                 (transform/scale 0.5 0.5 0.5))]
      (is (contains? (:light-sources world)
                     (light-sources/create-point-light (point/point -10 10 -10)
                                                       [1 1 1])))
      (is (some #(ray/same-sphere? expected-sphere1 %) (:objects world)))
      (is (some #(ray/same-sphere? expected-sphere2 %) (:objects world))))))

(deftest test-intersect
  (testing "Intersect a world with a ray"
    (let [world (world/default-world)
          ray (ray/ray (point/point 0 0 -5)
                       (svector/svector 0 0 1))]
      (is (v= [4 4.5 5.5 6]
              (map :t (world/intersect world ray)))))))

(deftest test-prepare-computation
  (testing "Precomputing the state of an intersection"
    (let [ray (ray/ray (point/point 0 0 -5)
                       (svector/svector 0 0 1))
          intersection (ray/intersection 4 (ray/sphere))
          result (world/prepare-computations ray intersection)]
      (is (ray/same-sphere? (ray/sphere) (:object result)))
      (is (= 4 (:t result)))
      (is (v= (point/point 0 0 -1) (:point result)))
      (is (v= (svector/svector 0 0 -1) (:eye-v result)))
      (is (v= (svector/svector 0 0 -1) (:normal-v result)))
      (is (not (:inside result)))))
  (testing "The hit, when an intersection occurs on the outside"
    (let [ray (ray/ray (point/point 0 0 -5)
                       (svector/svector 0 0 1))
          intersection (ray/intersection 4 (ray/sphere))]
      (is (not (:inside (world/prepare-computations ray intersection))))))

  
  (testing "The hit, when an intersection occurs on the inside"
    (let [ray (ray/ray (point/point 0 0 0)
                       (svector/svector 0 0 1))
          intersection (ray/intersection 1 (ray/sphere))
          result (world/prepare-computations ray intersection)]
      (is (:inside result))
      (is (v= (point/point 0 0 1) (:point result)))
      (is (v= (svector/svector 0 0 -1) (:eye-v result)))
      (is (v= (svector/svector 0 0 -1) (:normal-v result))))))

(deftest test-shade-hit
  (testing "Shading an intersection"
    (let [world (world/default-world)
          intermediate (world/prepare-computations (ray/ray (point/point 0 0 -5)
                                                            (svector/svector 0 0 1))
                                                   (ray/intersection 4 (first (:objects world))))]
      (is (v= [0.38066 0.47583 0.2855]
              (world/shade-hit world
                               intermediate)))))
  (testing "Shading an intersection from the inside"
    (let [world (world/set-light-sources (world/default-world)
                                         (light-sources/create-point-light (point/point 0 0.25 0)
                                                                           [1 1 1]))
          intermediate (world/prepare-computations (ray/ray (point/point 0 0 0)
                                                            (svector/svector 0 0 1))
                                                   (ray/intersection 0.5 (second (:objects world))))]
      (is (v= [0.90498 0.90498 0.90498]
              (world/shade-hit world
                               intermediate))))))

(defn reset-ambient-color [object]
  (let [new-material (assoc (:material object) :ambient 1)]
   (assoc object :material new-material)))

(deftest test-color-at
  (testing "The color when a ray misses"
    (is (v= [0 0 0]
            (world/color-at (world/default-world)
                            (ray/ray (point/point 0 0 -5)
                                     (svector/svector 0 1 0))))))
  (testing "The color when a ray hits"
    (is (v= [0.38066 0.47583 0.2855]
            (world/color-at (world/default-world)
                            (ray/ray (point/point 0 0 -5)
                                     (svector/svector 0 0 1))))))
  (testing "The color with an intersection behind the ray"
    (let [world (world/set-objects (world/default-world)
                                   (map reset-ambient-color (:objects (world/default-world))))
          ray (ray/ray (point/point 0 0 0.75) (svector/svector 0 0 -1))]
      (is (v= (:color (:material (second (:objects world))))
              (world/color-at world ray))))))

