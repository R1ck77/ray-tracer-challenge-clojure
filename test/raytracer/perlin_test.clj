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


(deftest test-get-cell
  (let [perlin-data {:y-scale 3
                    :x-scale 5
                     :grid nil}
        yinc (/ 3)
        xinc (/ 5)]
    (testing "get-cell returns the correct indices for a point inside the grid"
      (is (= [0 0] (perlin/get-cell perlin-data [0.0001 0.0001])))
      (is (= [2 4] (perlin/get-cell perlin-data [(+ (* xinc 4)
                                                    0.0001)
                                                 (+ (* yinc 2)
                                                    0.0001)])))
      (is (= [1 2] (perlin/get-cell perlin-data [(+ (* xinc 2)
                                                    0.0001)
                                                 (+ (* yinc 1)
                                                    0.0001)]))))
    (testing "get-cell returns the correct indices for a point outside the grid"
      (is (= [7 8] (perlin/get-cell perlin-data [(+ (* xinc 8)
                                                    0.0001)
                                                 (+ (* yinc 7)
                                                    0.0001)])))
      (is (= [-10 -2] (perlin/get-cell perlin-data [(+ (* xinc -2)
                                                    0.0001)
                                                 (+ (* yinc -10)
                                                    0.0001)]))))))
