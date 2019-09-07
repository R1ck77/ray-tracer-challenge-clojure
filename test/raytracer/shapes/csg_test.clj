(ns raytracer.shapes.csg-test
  (:require [clojure.test :refer :all]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.ray :as ray]
            [raytracer.transform :as transform]
            [raytracer.intersection :as intersection]
            [raytracer.shapes :as shapes]
            [raytracer.shapes.placement :as placement]            
            [raytracer.test-utils :as tu]
            [raytracer.intersection :as intersection]
            [raytracer.shapes.shared :as shared]
            [raytracer.shapes.csg :as csg]))

(deftest test-union-intersection-allowed?
  (testing "Rules for the CSG union operation"
    (let [union (csg/union nil nil)]
      (is (not (csg/is-intersection-allowed? union true true true)))
      (is (csg/is-intersection-allowed? union true true false))
      (is (not (csg/is-intersection-allowed? union true false true)))
      (is (csg/is-intersection-allowed? union true false false))
      (is (not (csg/is-intersection-allowed? union false true true)))
      (is (not (csg/is-intersection-allowed? union false true false)))
      (is (csg/is-intersection-allowed? union false false true))
      (is (csg/is-intersection-allowed? union false false false))))
  (testing "Rules for the CSG intersection operation"
    (let [intersection (csg/intersection nil nil)]
      (is (csg/is-intersection-allowed? intersection true true true))
      (not (csg/is-intersection-allowed? intersection true true false))
      (is (csg/is-intersection-allowed? intersection true false true))
      (not (csg/is-intersection-allowed? intersection true false false))
      (is (csg/is-intersection-allowed? intersection false true true))
      (is (csg/is-intersection-allowed? intersection false true false))
      (not (csg/is-intersection-allowed? intersection false false true))
      (not (csg/is-intersection-allowed? intersection false false false))))
  (testing "Rules for the CSG difference operation"
    (let [difference (csg/difference nil nil)]
      (is (not (csg/is-intersection-allowed? difference true true true)))
      (is (csg/is-intersection-allowed? difference true true false))
      (is (not (csg/is-intersection-allowed? difference true false true)))
      (is (csg/is-intersection-allowed? difference true false false))
      (is (csg/is-intersection-allowed? difference false true true))
      (is (csg/is-intersection-allowed? difference false true false))
      (is (not (csg/is-intersection-allowed? difference false false true)))
      (is (not (csg/is-intersection-allowed? difference false false false))))))

(deftest test-filter-intersections
  (let [sphere (shapes/sphere)
        cube (shapes/cube)
        int-0 (intersection/intersection 1 sphere)
        int-1 (intersection/intersection 2 cube)
        int-2 (intersection/intersection 3 sphere)
        int-3 (intersection/intersection 4 cube)
        intersections [int-0 int-1 int-2 int-3]]
    (testing "Filtering a list of intersections"
      (is (= [int-0 int-3]
             (csg/filter-intersections (csg/union sphere cube) intersections)))
      (is (= [int-1 int-2]
             (csg/filter-intersections (csg/intersection sphere cube) intersections)))
      (is (= [int-0 int-1]
             (csg/filter-intersections (csg/difference sphere cube) intersections))))))

(deftest test-union-includes?
  (let [cube (shapes/cube)
        sphere (shapes/sphere)
        another-sphere (shapes/sphere)
        csg-shape (csg/union cube sphere)]
    (testing "An union CSG shape includes itself and its own objects"
      (is (shared/includes? csg-shape csg-shape))
      (is (shared/includes? csg-shape cube))
      (is (shared/includes? csg-shape sphere)))
    (testing "An union CSG shape does not include extra objects"
      (is (not (shared/includes? csg-shape another-sphere))))))

(deftest test-intersection-includes?
  (let [cube (shapes/cube)
        sphere (shapes/sphere)
        another-sphere (shapes/sphere)
        csg-shape (csg/intersection cube sphere)]
    (testing "An intersection CSG shape includes itself and its own objects"
      (is (shared/includes? csg-shape csg-shape))
      (is (shared/includes? csg-shape cube))
      (is (shared/includes? csg-shape sphere)))
    (testing "An intersection CSG shape does not include extra objects"
      (is (not (shared/includes? csg-shape another-sphere))))))

(deftest test-difference-includes?
  (let [cube (shapes/cube)
        sphere (shapes/sphere)
        another-sphere (shapes/sphere)
        csg-shape (csg/difference cube sphere)]
    (testing "A difference CSG shape includes itself and its own objects"
      (is (shared/includes? csg-shape csg-shape))
      (is (shared/includes? csg-shape cube))
      (is (shared/includes? csg-shape sphere)))
    (testing "A difference CSG shape does not include extra objects"
      (is (not (shared/includes? csg-shape another-sphere))))))

(deftest test-local-intersect
  (testing "A ray misses a CSG object"
    (let [csg-shape (csg/union (shapes/sphere) (shapes/cube))
          ray (ray/ray (point/point 0 2 -5)
                       (svector/svector 0 0 1))]
      (is (empty? (shared/local-intersect csg-shape ray)))))
  (testing "A ray hits a CSG object"
    (let [sphere (shapes/sphere)
          shifted-sphere (placement/change-shape-transform sphere (transform/translate 0 0 0.5))
          csg-shape (csg/union sphere shifted-sphere)
          ray (ray/ray (point/point 0 0 -5)
                       (svector/svector 0 0 1))
          intersections (shared/local-intersect csg-shape ray)]
      (is (= [(intersection/intersection 4.0 sphere)
              (intersection/intersection 6.5 shifted-sphere)]
             intersections)))))
