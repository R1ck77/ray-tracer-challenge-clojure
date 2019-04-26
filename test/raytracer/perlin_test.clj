(ns raytracer.perlin-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.svector :as svector]
            [raytracer.perlin :as perlin]))

(deftest test-create-grid
  (let [width 5
        height 3]
    (testing "creates an array of svectors of the right size"
      (let [grid (perlin/create-grid width height)]
        (is (= height (count grid)))
        (is (= width (count (aget grid 0))))
        (is (= width (count (aget grid 1))))
        (is (= width (count (aget grid 2))))))
    (testing "creates an array in which each element is a unit length 3D vector"
      (let [grid (perlin/create-grid width height)]
        (doseq [i (range height)
                j (range width)]
          (is (eps= 1 (svector/mag (aget grid i j)))))))
    (testing "creates an array in which each 3D vector has a 0 z component"
      (let [grid (perlin/create-grid width height)]
        (doseq [i (range height)
                j (range width)]
          (is (eps= 0 (nth (aget grid i j) 2))))))))

(deftest test-create-perlin-data
  (testing "creates a perlin structure with the grid and the grid dimensions readily available"
    (let [perlin-data (perlin/create-perlin-data 30 20)]
      (is (= 30 (:x-scale perlin-data)))
      (is (= 20 (:y-scale perlin-data)))
      (is (= 20 (count (:grid perlin-data))))
      (is (= 30 (count (aget (:grid perlin-data) 0)))))))

(deftest test-get-cell-corner
  (let [perlin-data (perlin/create-perlin-data 5 3)]
    (testing "get-cell-corner returns the correct indices for a scaled point inside the grid"
      (is (= [0 0] (perlin/get-cell-corner perlin-data [0.1 0.1])))
      (is (= [4 2] (perlin/get-cell-corner perlin-data [4.1 2.1])))
      (is (= [2 1] (perlin/get-cell-corner perlin-data [2.1 1.1]))))
    (testing "get-cell-corner returns the correct indices for a scaled point outside the grid"
      (is (= [3 1] (perlin/get-cell-corner perlin-data [8.1 7.1]))))))

(deftest test-scale-point
  (let [perlin-data (perlin/create-perlin-data 5 3)]
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
  (let [perlin-data (perlin/create-perlin-data 5 3)]
    (testing "conditions for point in the higher left corner"
      (is (= [[4 2] [0 2] [4 0] [0 0]]
             (perlin/get-neighbors perlin-data [4.9 2.9]))))
    (testing "condition for the point in the lower right corner"
      (is (= [[0 0] [1 0] [0 1] [1 1]]
             (perlin/get-neighbors perlin-data [0.5 0.5]))))))

(deftest test-get-distances
  (testing "given a point in the grid and a set of neighbors, compute the distances"
    (is (= [{:coords [0 0]
              :distance (svector/svector 0 0 0)}
             {:coords [0 1]
              :distance (svector/svector 0 1 0)}
             {:coords [1 0]
              :distance (svector/svector 1 0 0)}
             {:coords [1 1]
              :distance (svector/svector 1 1 0)}]
           (perlin/compute-distances [[0 0] [0 1] [1 0] [1 1]]
                                     [0 0])))))


(defn- create-fake-perlin-data []
  (let [grid (perlin/create-grid 5 3)]
    (doseq [i (range 3), j (range 5)]
      (aset grid i j (svector/svector j i 0)))
    {:x-scale 5
     :y-scale 3
     :grid grid}))

(deftest test-get-products
  (let [perlin-data (create-fake-perlin-data)]
    (testing "given a set of distances and the grid, compute the dot products"
      (is (= [{:coords [0 0]
               :dot 0}
               {:coords [1 1]
                :dot 2}              
               {:coords [1 0]
                :dot 1}
               {:coords [0 1]
                :dot 1}]             
             (perlin/compute-products (:grid perlin-data) [{:coords [0 0]
                                                             :distance (svector/svector 0 0 0)}
                                                            {:coords [1 1]
                                                             :distance (svector/svector 1 1 0)}
                                                            {:coords [1 0]
                                                             :distance (svector/svector 1 0 0)}
                                                            {:coords [0 1]
                                                             :distance (svector/svector 0 1 0)}]))))))

(deftest test-noise
  (let [perlin-data (perlin/create-perlin-data 7 13)]
    (testing "noise returns some value for some special points"
      (is (perlin/noise perlin-data [0 0]) 0)
      (is (perlin/noise perlin-data [0.9999 0.9999]) 0))
    (testing "noise returns some value for points outside the grid"
      (is (perlin/noise perlin-data [-0.2 -0.4]) 0)
      (is (perlin/noise perlin-data [1.3 4.5]) 0))))
