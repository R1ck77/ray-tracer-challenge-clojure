(ns raytracer.perlin-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.svector :as svector]
            [raytracer.perlin :as perlin]))

(deftest test-create-grid
  (let [dimensions [5 3]]
    (testing "creates an array of svectors of the right size"
      (let [grid (perlin/create-grid dimensions)]
        (is (= 3 (count grid)))
        (is (= 5 (count (aget grid 0))))
        (is (= 5 (count (aget grid 1))))
        (is (= 5 (count (aget grid 2)))))
      (let [grid (perlin/create-grid [5 3 2])]
        (is (= 2 (count grid)))
        (is (= 3 (count (aget grid 0))))
        (is (= 5 (count (aget grid 0 0))))))
    (testing "creates an array in which each element is a unit length 3D vector"
      (let [grid (perlin/create-grid dimensions)]
        (doseq [i (range 3)
                j (range 5)]
          (is (eps= 1 (Math/sqrt (apply + (map #(* % %) (aget grid i j)))))))))))

(deftest test-create-perlin-data
  (testing "creates a perlin structure with the grid and the grid dimensions readily available"
    (let [perlin-data (perlin/create-perlin-data [30 20])]
      (is (= 30 (:x-scale perlin-data)))
      (is (= 20 (:y-scale perlin-data)))
      (is (= 20 (count (:grid perlin-data))))
      (is (= 30 (count (aget (:grid perlin-data) 0)))))))

(deftest test-scale-point
  (let [perlin-data (perlin/create-perlin-data [5 3])]
    (testing "positive point in range"
      (is (v= [1 1.8]
              (perlin/scale-point perlin-data [0.2 0.6])))
      (is (v= [2.5 1.5]
              (perlin/scale-point perlin-data [0.5 0.5]))))
    (testing "negative point"
      (is (v= [-4.5 -2.7]
              (perlin/scale-point perlin-data [-0.9 -0.9])))
      (is (v= [-19.5 -8.7]
              (perlin/scale-point perlin-data [-3.9 -2.9]))))))

(defn- set-fake-grid-values! [grid width height]
  (doseq [j (range height)
          i (range width)]
    (aset grid j i [(str i ";" j)])))

(deftest test-get-pbc
  (let [perlin-data (perlin/create-perlin-data [5 3])]
    (set-fake-grid-values! (:grid perlin-data) 5 3)
    (testing "return the correct value for cells in the grid"
      (is (= ["3;2"] (perlin/get-pbc perlin-data [3 2])))
      (is (= ["0;0"] (perlin/get-pbc perlin-data [0 0])))
      (is (= ["4;2"] (perlin/get-pbc perlin-data [4 2]))))
    (testing "return the correct value for cells outside the grid"
      (is (= ["3;1"] (perlin/get-pbc perlin-data [8 7])))
      (is (= ["1;1"] (perlin/get-pbc perlin-data [16 10])))
      (is (= ["0;1"] (perlin/get-pbc perlin-data [0 10])))
      (is (= ["2;0"] (perlin/get-pbc perlin-data [17 0]))))
    (testing "return the correct value for cells negative indices outside the grid"
      (is (= ["4;2"] (perlin/get-pbc perlin-data [-1 -1])))
      (is (= ["3;1"] (perlin/get-pbc perlin-data [-2 -2])))
      (is (= ["1;2"] (perlin/get-pbc perlin-data [-4 -4])))
      (is (= ["4;0"] (perlin/get-pbc perlin-data [-6 -6]))))))

(deftest test-get-scaled-point-bounds
  (testing "return the correct indices for a scaled point inside the grid"
    (is (= {:corners [[0 0] [1 0] [0 1] [1 1]]
            :point [0.1 0.1]}
           (perlin/get-scaled-point-bounds [0.1 0.1])))
    (is (= {:corners [[4 2] [5 2] [4 3] [5 3]]
            :point [4.1 2.1]}
           (perlin/get-scaled-point-bounds [4.1 2.1])))
    (is (= {:corners [[2 1] [3 1] [2 2] [3 2]]
            :point [2.1 1.1]}
           (perlin/get-scaled-point-bounds [2.1 1.1]))))
  (testing "return the correct indices for a scaled point outside the grid"
    (is (= {:corners [[8 7] [9 7] [8 8] [9 8]]
            :point [8.1 7.1]} (perlin/get-scaled-point-bounds [8.1 7.1])))
    (is (= {:corners [[-2 -3] [-1 -3] [-2 -2] [-1 -2]]
            :point [-1.3 -2.4]} (perlin/get-scaled-point-bounds [-1.3 -2.4])))))

(deftest test-assoc-dot-products
  (testing "computes the dot product for the specific point"
    (let [perlin-data (perlin/create-perlin-data [6 4])]
      (aset (:grid perlin-data) 2 4 [1 2])
      (aset (:grid perlin-data) 2 5 [3 4])
      (aset (:grid perlin-data) 3 4 [-1 -2])
      (aset (:grid perlin-data) 3 5 [-3 -4])
      (is (= {:corners [[4 2] [5 2] [4 3] [5 3]]
              :point [4 2]
              :dots [0 3 -2 -7]}
             (perlin/assoc-dot-products perlin-data
                                          {:corners [[4 2] [5 2] [4 3] [5 3]]
                                           :point [4 2]}))))))

(deftest test-interpolate
  (testing "computes an interpolation for the point"
    (is (eps= -1.887875
              (perlin/interpolate {:corners [[4 2] [5 2] [4 3] [5 3]]
                                   :dots [0 3 -2 -7]
                                   :point [4.2 2.7]})))))

(deftest test-noise
  (testing "can produce a value of sorts"
    (is (not (nil? (perlin/noise (perlin/create-perlin-data [6 4])
                                 [0.1 0.7]))))))
