(ns raytracer.world-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.world :as world]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]
            [raytracer.ray :as ray]
            [raytracer.intersection :as intersection]
            [raytracer.shapes :as shapes]
            [raytracer.materials :as materials]
            [raytracer.transform :as transform]
            [raytracer.light-sources :as light-sources]))

(def √2 (Math/sqrt 2))
(def half√2 (/ √2 2))

(deftest test-create
  (testing "Creating a world"
    (let [world (world/create)]
      (is (empty? (:objects world)))
      (is (empty? (:light-sources world))))))

(deftest test-add-object
  (testing "Adding a new object"
    (let [world (world/default-world)
          new-object (shapes/plane)]
      (is (contains? (apply hash-set (:objects (world/add-object world new-object)))
                     new-object)))))

(deftest test-default-world
  (testing "The default world"
    (let [world (world/default-world)
          expected-sphere1 (shapes/change-material (shapes/sphere)
                                                   (materials/material :color [0.8 1.0 0.6]
                                                                       :diffuse 0.7
                                                                       :specular 0.2))
          expected-sphere2 (shapes/change-transform (shapes/sphere)
                                                    (transform/scale 0.5 0.5 0.5))]
      (is (contains? (:light-sources world)
                     (light-sources/create-point-light (point/point -10 10 -10)
                                                       [1 1 1])))
      (is (some #(= expected-sphere1 %) (:objects world)))
      (is (some #(= expected-sphere2 %) (:objects world))))))

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
          intersection (intersection/intersection 4 (shapes/sphere))
          result (world/prepare-computations ray intersection)]
      (is (= (shapes/sphere) (:object result)))
      (is (= 4 (:t result)))
      (is (v= (point/point 0 0 -1) (:point result)))
      (is (v= (svector/svector 0 0 -1) (:eye-v result)))
      (is (v= (svector/svector 0 0 -1) (:normal-v result)))
      (is (not (:inside result)))))
  (testing "The hit, when an intersection occurs on the outside"
    (let [ray (ray/ray (point/point 0 0 -5)
                       (svector/svector 0 0 1))
          intersection (intersection/intersection 4 (shapes/sphere))]
      (is (not (:inside (world/prepare-computations ray intersection))))))
  (testing "The hit, when an intersection occurs on the inside"
    (let [ray (ray/ray (point/point 0 0 0)
                       (svector/svector 0 0 1))
          intersection (intersection/intersection 1 (shapes/sphere))
          result (world/prepare-computations ray intersection)]
      (is (:inside result))
      (is (v= (point/point 0 0 1) (:point result)))
      (is (v= (svector/svector 0 0 -1) (:eye-v result)))
      (is (v= (svector/svector 0 0 -1) (:normal-v result)))))
  (testing "Precomputing the reflection vector"
    (let [shape (shapes/plane)
          ray (ray/ray (point/point 0 1 -1)
                     (svector/svector 0 (- half√2) half√2))
          intersection (intersection/intersection √2 shape)]
      (is (v= (svector/svector 0 half√2 half√2)
              (:reflection (world/prepare-computations ray intersection))))))
  (testing "The hit should offset the point"
    (let [ray (ray/ray (point/point 0 0 -5)
                       (svector/svector 0 0 1))
          sphere (shapes/change-transform (shapes/sphere)
                                          (transform/translate 0 0 1))
          intersection (intersection/intersection 5 sphere)
          intermediate (world/prepare-computations ray intersection)
          ]
      (is (< (nth (:over-point intermediate) 2)
             (/ (- world/EPSILON) 2)))
      (is (> (nth (:point intermediate) 2)
             (nth (:over-point intermediate) 2))))))

(deftest test-shade-hit
  (testing "Shading an intersection"
    (let [world (world/default-world)
          intermediate (world/prepare-computations (ray/ray (point/point 0 0 -5)
                                                            (svector/svector 0 0 1))
                                                   (intersection/intersection 4 (first (:objects world))))]
      (is (v= [0.38066 0.47583 0.2855]
              (world/shade-hit world
                               intermediate)))))
  (testing "Shading an intersection from the inside"
    (let [world (world/set-light-sources (world/default-world)
                                         (light-sources/create-point-light (point/point 0 0.25 0)
                                                                           [1 1 1]))
          intermediate (world/prepare-computations (ray/ray (point/point 0 0 0)
                                                            (svector/svector 0 0 1))
                                                   (intersection/intersection 0.5 (second (:objects world))))]
      (is (v= [0.90498 0.90498 0.90498]
              (world/shade-hit world
                               intermediate)))))
  (testing "shade_hit() is given an intersection in shadow"
    (let [ sphere1 (shapes/sphere)
          sphere2 (shapes/change-transform (shapes/sphere)
                                           (transform/translate 0 0 10))
          world (-> (world/create)
                    (world/set-light-sources (light-sources/create-point-light (point/point 0 0 -10) [1 1 1]))
                    (world/set-objects [sphere1 sphere2]))
          ray (ray/ray (point/point 0 0 5)
                       (svector/svector 0 0 1))
          intersection (intersection/intersection 4 sphere2)
          comp (world/prepare-computations ray intersection)
          color (world/shade-hit world comp)]
      (is (v= [0.1 0.1 0.1]
              color))))
    (testing "Lighting with reflection enabled"
      (let [template-shape (shapes/plane)
            shape (shapes/change-material (shapes/change-transform template-shape
                                                                   (transform/translate 0 -1 0))
                                          (materials/update-material (:material template-shape)
                                                                     :reflectivity 0.5))
            world (world/add-object (world/default-world) shape)
            ray (ray/ray (point/point 0 0 -3)
                         (svector/svector 0 (- half√2) half√2))
            intersection (intersection/intersection √2 shape)]
        (is (v= [0.87677 0.92436 0.82918]
                (world/shade-hit world (world/prepare-computations ray intersection)))))))

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
              (world/color-at world ray)))))
  (testing "color_at() with mutually reflective surfaces"
    (let [light (light-sources/create-point-light (point/point 0 0 0)
                                                  [1 1 1])
          reflective-plane (shapes/change-material (shapes/plane)
                                                   (materials/material :reflectivity 1))
          lower-plane (shapes/change-transform reflective-plane (transform/translate 0 -1 0))
          upper-plane (shapes/change-transform reflective-plane (transform/translate 0 1 0))
          world (-> (world/default-world)
                    (world/set-light-sources light)
                    (world/set-objects [lower-plane upper-plane]))]
      (is (world/color-at world (ray/ray (point/point 0 0 0)
                                         (svector/svector 0 1 0)))))))
(deftest test-view-transform
  (testing "The transformation matrix for the default orientation"
    (let [from (point/point 0 0 0)
          to (point/point 0 0 -1)
          up (svector/svector 0 1 0)]
      (is (v= matrix/identity-matrix
              (world/view-transform from to up)))))
  (testing "A view transformation matrix looking in positive z direction"
    (let [from (point/point 0 0 0)
          to (point/point 0 0 1)
          up (svector/svector 0 1 0)]
      (is (v= (transform/scale -1 1 -1)
              (world/view-transform from to up)))))
  (testing "The view transformation moves the world"
    (let [from (point/point 0 0 8)
          to (point/point 0 0 0)
          up (svector/svector 0 1 0)]
      (is (v= (transform/translate 0 0 -8)
              (world/view-transform from to up)))))
  (testing "An arbitrary view transformation"
    (let [from (point/point 1 3 2)
          to (point/point 4 -2 8)
          up (svector/svector 1 1 0)]
      (is (v= [-0.50709 0.50709 0.67612 -2.36643
               0.76772 0.60609 0.12122 -2.82843
               -0.35857 0.59761 -0.71714 0.00000
               0.00000 0.00000 0.00000 1.00000]
              (world/view-transform from to up))))))

(deftest test-is-shadowed?
  (let [world (world/default-world)]
    (testing "There is no shadow when nothing is collinear with point and light"      
      (is (not (world/is-shadowed? world (point/point 0 10 0)))))
    (testing "The shadow when an object is between the point and the light"
      (is (world/is-shadowed? world (point/point 10 -10 10))))
    (testing "There is no shadow when an object is behind the light"
      (is (not (world/is-shadowed? world (point/point -20 20 -20)))))
    (testing "There is no shadow when an object is behind the point"
      (is (not (world/is-shadowed? world (point/point -2 2 -2)))))))

(defn- update-ambient [shape]
  (shapes/change-material shape
                          (materials/update-material (:material shape)
                                                     :ambient 1)))

(defn- change-second-object-material [world]
  (let [objects (:objects world)]
    (world/set-objects world
                       (assoc objects 1 (update-ambient (second objects))))))

(deftest test-reflected-color
  (testing "The reflected color for a nonreflective material"
    (let [world (change-second-object-material (world/default-world))
          ray (ray/ray (point/point 0 0 0)
                       (svector/svector 0 0 1))
          intersection (intersection/intersection 1 (second (:objects world)))]
      (is (v= [0 0 0]
              (world/reflected-color world
                                     (world/prepare-computations ray
                                                                 intersection))))))

  (testing "The reflected color for a reflective material"
    (let [template-shape (shapes/plane)
          shape (shapes/change-material (shapes/change-transform template-shape
                                                                 (transform/translate 0 -1 0))
                                        (materials/update-material (:material template-shape)
                                                                   :reflectivity 0.5))
          world (world/add-object (world/default-world) shape)
          ray (ray/ray (point/point 0 0 -3)
                       (svector/svector 0 (- half√2) half√2))
          intersection (intersection/intersection √2 shape)]
      (is (v= [0.19032 0.2379 0.14274]
              (world/reflected-color world (world/prepare-computations ray intersection)))))))
