(ns raytracer.shapes.csg-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :as tu]
            [raytracer.shapes.csg :as csg]))


(deftest test-union-intersection-allowed?
  (testing "Rules for the CSG union operation"
    (let [union (csg/union)]
      (is (not (csg/is-intersection-allowed? union true true true)))
      (is (csg/is-intersection-allowed? union true true false))
      (is (not (csg/is-intersection-allowed? union true false true)))
      (is (csg/is-intersection-allowed? union true false false))
      (is (not (csg/is-intersection-allowed? union false true true)))
      (is (not (csg/is-intersection-allowed? union false true false)))
      (is (csg/is-intersection-allowed? union false false true))
      (is (csg/is-intersection-allowed? union false false false))))
  (testing "Rules for the CSG intersection operation"
    (let [intersection (csg/intersection)]
      (is (csg/is-intersection-allowed? intersection true true true))
      (not (csg/is-intersection-allowed? intersection true true false))
      (is (csg/is-intersection-allowed? intersection true false true))
      (not (csg/is-intersection-allowed? intersection true false false))
      (is (csg/is-intersection-allowed? intersection false true true))
      (is (csg/is-intersection-allowed? intersection false true false))
      (not (csg/is-intersection-allowed? intersection false false true))
      (not (csg/is-intersection-allowed? intersection false false false))))
  (testing "Rules for the CSG difference operation"
    (let [difference (csg/difference)]
      (is (not (csg/is-intersection-allowed? difference true true true)))
      (is (csg/is-intersection-allowed? difference true true false))
      (is (not (csg/is-intersection-allowed? difference true false true)))
      (is (csg/is-intersection-allowed? difference true false false))
      (is (csg/is-intersection-allowed? difference false true true))
      (is (csg/is-intersection-allowed? difference false true false))
      (is (not (csg/is-intersection-allowed? difference false false true)))
      (is (not (csg/is-intersection-allowed? difference false false false))))))
