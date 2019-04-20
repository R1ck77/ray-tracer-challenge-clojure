(ns raytracer.ray-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]            
            [raytracer.ray :as ray]
            [raytracer.shapes :as shapes]
            [raytracer.transform :as transform]
            [raytracer.materials :as materials]))

(deftest test-ray
  (testing "ray creation"
    (let [origin (point/point 1 2 3)
          direction (svector/svector 4 5 6)
          ray (ray/ray origin direction)]
      (is (identical? origin (:origin ray)))
      (is (identical? direction (:direction ray))))))

(deftest test-position
  (let [ray (ray/ray (point/point 2 3 4)
                     (svector/svector 1 0 0))]
    (testing "t=0"
      (is (v= (point/point 2 3 4)
              (ray/position ray 0))))
    (testing "t=1"
      (is (v= (point/point 3 3 4)
              (ray/position ray 1))))
    (testing "t=-1"
      (is (v= (point/point 1 3 4)
              (ray/position ray -1))))
    (testing "t=2.5"
      (is (v= (point/point 4.5 3 4)
              (ray/position ray 2.5))))))

(deftest test-sphere
  (let [sphere (shapes/sphere)]
    (testing "create a sphere"
      (is (v= [0 0 0 1] (:center sphere)))
      (is (eps= 1.0 (:radius sphere)))
      (is (= (materials/material) (:material sphere)))
      (is (identical? matrix/identity-matrix (:transform sphere))))
    (testing "update a sphere's transformation"
      (let [transform (transform/translate 2 3 4)
            new-sphere (shapes/change-transform sphere transform)]
        (is (identical? transform
                        (:transform new-sphere)))))
    (testing "a sphere may be assigned a material"
      (let [new-material (assoc (materials/material) :ambient 0.25)]
        (is (= new-material
               (:material (shapes/change-material sphere new-material))))))))

(deftest test-same-shape?
  (testing "returns the first argument if true, nil otherwise"
    (let [a (shapes/change-transform (shapes/sphere) (transform/scale 2 2 2))
          b (shapes/change-transform (shapes/sphere) (transform/scale 2 2 2))]
      (is (not (identical? a b)))
      (is (identical? a (shapes/same-shape? a b)))
      (is (nil? (shapes/same-shape? a (shapes/sphere))))))
  (testing "two spheres are equal when they receive the same transform"
    (is (shapes/same-shape? (shapes/change-transform (shapes/sphere) (transform/scale 2 2 2))
                            (shapes/change-transform (shapes/sphere) (transform/scale 2 2 2)))))
  (testing "different transforms, different sphere"
    (is (not (shapes/same-shape? (shapes/change-transform (shapes/sphere) (transform/scale 2 2 3))
                                 (shapes/change-transform (shapes/sphere) (transform/scale 2 2 2)))))))

(deftest test-sphere-intersection
  (let [sphere (shapes/sphere)]
    (testing "a ray intersects a sphere at two points"
      (let [intersections (ray/intersect (ray/ray (point/point 0 0 -5)
                                                  (svector/svector 0 0 1))
                                         sphere)]
        (is (= 2 (count intersections)))
        (is (shapes/same-shape? sphere (:object (first intersections))))
        (is (shapes/same-shape? sphere (:object (second intersections))))
        (is (eps= 4 (:t (first intersections))))
        (is (eps= 6 (:t (second intersections))))))
    (testing "a ray intersects a sphere at a tangent"
      (let [intersections (ray/intersect (ray/ray (point/point 0 1 -5)
                                                  (svector/svector 0 0 1))
                                         sphere)]
        (is (= 2 (count intersections)))
        (is (shapes/same-shape? sphere (:object (first intersections))))
        (is (shapes/same-shape? sphere (:object (second intersections))))
        (is (eps= 5 (:t (first intersections))))
        (is (eps= 5 (:t (second intersections))))))
    (testing "a ray misses a sphere"
      (let [intersections (ray/intersect (ray/ray (point/point 0 2 -5)
                                                  (svector/svector 0 0 1))
                                         sphere)]
        (is (= 0 (count intersections)))))
    (testing "a ray originates inside a sphere"
      (let [intersections (ray/intersect (ray/ray (point/point 0 0 0)
                                                  (svector/svector 0 0 1))
                                         sphere)]
        (is (= 2 (count intersections)))
        (is (shapes/same-shape? sphere (:object (first intersections))))
        (is (shapes/same-shape? sphere (:object (second intersections))))
        (is (eps= -1 (:t (first intersections))))
        (is (eps= 1 (:t (second intersections))))))
    (testing "a sphere is behind a ray"
      (let [intersections (ray/intersect (ray/ray (point/point 0 0 5)
                                                  (svector/svector 0 0 1))
                                         sphere)]
        (is (= 2 (count intersections)))
        (is (shapes/same-shape? sphere (:object (first intersections))))
        (is (shapes/same-shape? sphere (:object (second intersections))))
        (is (eps= -6 (:t (first intersections))))
        (is (eps= -4 (:t (second intersections))))))
    (testing "intersecting a scaled sphere with a ray"
      (let [ray (ray/ray (point/point 0 0 -5)
                         (svector/svector 0 0 1))
            sphere (shapes/change-transform (shapes/sphere)
                                            (transform/scale 2 2 2))
            intersections (ray/intersect ray sphere)]
        (is (= 2 (count intersections)))
        (is (eps= 3 (:t (first intersections))))
        (is (eps= 7 (:t (second intersections))))))
    (testing "intersecting a translated sphere with a ray"
      (let [ray (ray/ray (point/point 0 0 -5)
                         (svector/svector 0 0 1))
            sphere (shapes/change-transform (shapes/sphere)
                                            (transform/translate 5 0 0))
            intersections (ray/intersect ray sphere)]
        (is (= 0 (count intersections)))))))

(deftest test-transform
  (let [ray (ray/ray (point/point 1 2 3)
                     (svector/svector 0 1 0))]
    (testing "translating a ray"
      (let [result (ray/transform ray (transform/translate 3 4 5))]
        (is (v= [4 6 8 1] (:origin result)))
        (is (v= [0 1 0 0] (:direction result)))))
    (testing "scaling a ray"
      (let [result (ray/transform ray (transform/scale 2 3 4))]
        (is (v= [2 6 12 1] (:origin result)))
        (is (v= [0 3 0 0] (:direction result)))))))

(deftest test-sphere-normal
  (let [sphere (shapes/sphere)
        √2 (Math/sqrt 2)
        half√2 (/ √2 2)
        √3 (Math/sqrt 3)
        third√3 (/ √3 3)]
    (testing "The normal on a sphere at a point on the x axis"
      (is (v= (svector/svector 1 0 0)
              ((:normal sphere) (point/point 1 0 0)))))
    (testing "The normal on a sphere at a point on the y axis"
      (is (v= (svector/svector 0 1 0)
              ((:normal sphere) (point/point 0 1 0)))))
    (testing "The normal on a sphere at a point on the z axis"
      (is (v= (svector/svector 0 0 1)
              ((:normal sphere) (point/point 0 0 1)))))
    (testing "The normal on a sphere at a nonaxial point"
      (is (v= (svector/svector third√3 third√3 third√3)
              ((:normal sphere) (point/point third√3 third√3 third√3)))))
    (testing "Computing the normal on a translated sphere"
      (let [transformed-sphere (shapes/change-transform sphere (transform/translate 0 1 0))]
        (is (v= (svector/svector 0 0.70711 -0.70711)
                ((:normal transformed-sphere) (point/point 0 1.70711 -0.70711))))))
    (testing "Computing the normal on a transformed sphere"
      (let [new-transform (transform/scale 1 0.5 1 (transform/rotate-z (/ Math/PI 5)))
            transformed-sphere (shapes/change-transform sphere new-transform)]
        (is (v= (svector/svector 0 0.97014 -0.24254)
                ((:normal transformed-sphere) (point/point 0 half√2 (- half√2)))))))))

