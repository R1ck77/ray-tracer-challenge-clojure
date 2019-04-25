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
                     :grid nil}]
    (testing "get-cell returns the correct indices for a point inside the grid"
      (is (= [0 0] (perlin/get-cell perlin-data [0.1 0.1])))
      (is (= [2 4] (perlin/get-cell perlin-data [4.1 2.1])))
      (is (= [1 2] (perlin/get-cell perlin-data [2.1 1.1]))))
    (testing "get-cell returns the correct indices for a point outside the grid"
      (is (= [1 3] (perlin/get-cell perlin-data [8.1 7.1]))))))

(deftest test-scale-point
  (let [perlin-data {:x-scale 5
                     :y-scale 3                     
                     :grid nil}]
    (testing "positive point in range"
      (is (v= [1 1.8]
              (perlin/scale-point perlin-data [0.2 0.6])))
      (is (v= [2.5 1.5]
              (perlin/scale-point perlin-data [0.5 0.5]))))
    (testing "negative point"
      (is (v= [(- 5 4.5) (- 3 2.7)]
              (perlin/scale-point perlin-data [-0.9 -0.9])))
      (is (v= [(- 20 19.5) (- 9 8.7)]
              (perlin/scale-point perlin-data [-3.9 -2.9]))))))

(deftest test-get-neighbors
  (let [perlin-data {:x-scale 5
                     :y-scale 3}]
    (testing "conditions for point in the higher left corner"
      (is (= #{[2 4] [0 4] [2 0] [0 0]}
             (perlin/get-neighbors perlin-data [4.9 2.9]))))
    (testing "condition for the point in the lower right corner"
      (is (= #{[0 0] [1 0] [0 1] [1 1]}
             (perlin/get-neighbors perlin-data [0.5 0.5]))))))

