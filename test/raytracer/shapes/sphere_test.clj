(ns raytracer.shapes.sphere-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.svector :as svector]
            [raytracer.shapes :as shapes]
            [raytracer.shapes.shared :as shared]
            [raytracer.point :as point]
            [raytracer.matrix :as matrix]
            [raytracer.ray :as ray]
            [raytracer.transform :as transform]
            [raytracer.materials :as materials]
            [raytracer.shapes.sphere :as sphere]))

(deftest test-sphere
  (let [sphere (shapes/sphere)]
    (testing "create a sphere"
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

(deftest test-sphere-intersection
  (let [sphere (shapes/sphere)]
    (testing "a ray intersects a sphere at two points"
      (let [intersections (ray/intersect (ray/ray (point/point 0 0 -5)
                                                  (svector/svector 0 0 1))
                                         sphere)]
        (is (= 2 (count intersections)))
        (is (= sphere (:object (first intersections))))
        (is (= sphere (:object (second intersections))))
        (is (eps= 4 (:t (first intersections))))
        (is (eps= 6 (:t (second intersections))))))
    (testing "a ray intersects a sphere at a tangent"
      (let [intersections (ray/intersect (ray/ray (point/point 0 1 -5)
                                                  (svector/svector 0 0 1))
                                         sphere)]
        (is (= 2 (count intersections)))
        (is (= sphere (:object (first intersections))))
        (is (= sphere (:object (second intersections))))
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
        (is (= sphere (:object (first intersections))))
        (is (= sphere (:object (second intersections))))
        (is (eps= -1 (:t (first intersections))))
        (is (eps= 1 (:t (second intersections))))))
    (testing "a sphere is behind a ray"
      (let [intersections (ray/intersect (ray/ray (point/point 0 0 5)
                                                  (svector/svector 0 0 1))
                                         sphere)]
        (is (= 2 (count intersections)))
        (is (= sphere (:object (first intersections))))
        (is (= sphere (:object (second intersections))))
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

(deftest test-sphere-normal
  (let [sphere (sphere/sphere)
        √2 (Math/sqrt 2)
        half√2 (/ √2 2)
        √3 (Math/sqrt 3)
        third√3 (/ √3 3)]
    (testing "The normal on a sphere at a point on the x axis"
      (is (t= (svector/svector 1 0 0)
              (shared/compute-normal sphere
               (point/point 1 0 0)))))
    (testing "The normal on a sphere at a point on the y axis"
      (is (t= (svector/svector 0 1 0)
              (shared/compute-normal sphere
               (point/point 0 1 0)))))
    (testing "The normal on a sphere at a point on the z axis"
      (is (t= (svector/svector 0 0 1)
              (shared/compute-normal sphere
               (point/point 0 0 1)))))
    (testing "The normal on a sphere at a nonaxial point"
      (is (t= (svector/svector third√3 third√3 third√3)
              (shared/compute-normal sphere
               (point/point third√3 third√3 third√3)))))
    (testing "Computing the normal on a translated sphere"
      (let [transformed-sphere (shapes/change-transform sphere (transform/translate 0 1 0))]
        (is (t= (svector/svector 0 0.70711 -0.70711)
                (shared/compute-normal transformed-sphere
                 (point/point 0 1.70711 -0.70711))))))
    (testing "Computing the normal on a transformed sphere"
      (let [new-transform (transform/scale 1 0.5 1 (transform/rotate-z (/ Math/PI 5)))
            transformed-sphere (shapes/change-transform sphere new-transform)]
        (is (t= (svector/svector 0 0.97014 -0.24254)
                (shared/compute-normal transformed-sphere
                 (point/point 0 half√2 (- half√2)))))))))
