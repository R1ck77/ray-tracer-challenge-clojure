(ns raytracer.perlin-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.svector :as svector]
            [raytracer.perlin :as perlin]))

(deftest test-create-grid
  (let [rows 3
        columns 5]
    (testing "create-grid creates an array of svectors of the right size"
      (let [grid (perlin/create-grid rows columns)]
        (is (= rows (count grid)))
        (is (= columns (count (aget grid 0))))
        (is (= columns (count (aget grid 1))))
        (is (= columns (count (aget grid 2))))))
    (testing "create-grid creates an array in which each element is a unit length 3D vector"
      (let [grid (perlin/create-grid rows columns)]
        (doseq [i (range rows)
                j (range columns)]
          (is (eps= 1 (svector/mag (aget grid i j)))))))
    (testing "create-grid creates an array in which each 3D vector has a 0 z component"
      (let [grid (perlin/create-grid rows columns)]
        (doseq [i (range rows)
                j (range columns)]
          (is (eps= 0 (nth (aget grid i j) 2))))))))
