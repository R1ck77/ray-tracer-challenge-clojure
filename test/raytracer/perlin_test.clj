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
      (is (= 30 (first (:dimensions perlin-data))))
      (is (= 20 (second (:dimensions perlin-data))))
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

(deftest test-compute-neighbors-displacements
  (testing "1D case"
    (is (= [[0] [1]]
           (perlin/compute-neighbors-displacements 1))))
  (testing "2D case"
    (is (= [[0 0] [1 0] [0 1] [1 1]]
           (perlin/compute-neighbors-displacements 2))))
  (testing "3D case"
    (is (= [[0 0 0] [1 0 0] [0 1 0] [1 1 0] [0 0 1] [1 0 1] [0 1 1] [1 1 1]]
           (perlin/compute-neighbors-displacements 3)))))

(deftest test-get-scaled-point-bounds
  (let [neighbors [[0 0] [1 0] [0 1] [1 1]]]
    (testing "return the correct indices for a scaled point inside the grid"
      (is (= {:corners [[0 0] [1 0] [0 1] [1 1]]
              :point [0.1 0.1]}
             (perlin/get-scaled-point-bounds neighbors [0.1 0.1])))
      (is (= {:corners [[4 2] [5 2] [4 3] [5 3]]
              :point [4.1 2.1]}
             (perlin/get-scaled-point-bounds neighbors [4.1 2.1])))
      (is (= {:corners [[2 1] [3 1] [2 2] [3 2]]
              :point [2.1 1.1]}
             (perlin/get-scaled-point-bounds neighbors [2.1 1.1]))))
    (testing "return the correct indices for a scaled point outside the grid"
      (is (= {:corners [[8 7] [9 7] [8 8] [9 8]]
              :point [8.1 7.1]} (perlin/get-scaled-point-bounds neighbors [8.1 7.1])))
      (is (= {:corners [[-2 -3] [-1 -3] [-2 -2] [-1 -2]]
              :point [-1.3 -2.4]} (perlin/get-scaled-point-bounds neighbors [-1.3 -2.4])))))
  (testing "3-dimensional case"
    (let [neighbors [[0 0 0] [1 0 0] [0 1 0] [1 1 0] [0 0 1] [1 0 1] [0 1 1] [1 1 1]]]
     (is (= {:corners [[8 7 12] [9 7 12] [8 8 12] [9 8 12] [8 7 13] [9 7 13] [8 8 13] [9 8 13]]
             :point [8.1 7.1 12.7]}
            (perlin/get-scaled-point-bounds neighbors [8.1 7.1 12.7]))))))

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

(defn- set-4D-grid-values!
  [{grid :grid} values]
  (dorun
   (map (fn [[[i j k w] v]]
          (aset grid w k j i v))
        values)))

(defn- set-3D-grid-values!
  [{grid :grid} values]
  (dorun
   (map (fn [[[i j k] v]]
          (aset grid k j i v))
        values)))

(defn- set-2D-grid-values!
  [{grid :grid} values]
  (dorun
   (map (fn [[[i j] v]]
          (aset grid j i v))
        values)))

(defn- set-1D-grid-values!
  [{grid :grid} values]
  (dorun
   (map-indexed #(aset grid % (vector %2)) values)))

(deftest test-normalization
  (testing "noise extremes for the 1D noise"
    (let [perlin-data (doto (perlin/create-perlin-data [2])
                        (set-1D-grid-values! [-1 1]))]
      (is (eps= 1 (perlin/noise perlin-data [0.25])))))
  (testing "noise extremes for the 2D noise"
    (let [half√2 (/ (Math/sqrt 2) 2)
          perlin-data (doto (perlin/create-perlin-data [2 2])
                        (set-2D-grid-values! {[0 0] [(- half√2) (- half√2)]
                                              [1 0] [(+ half√2) (- half√2)]
                                              [0 1] [(- half√2) (+ half√2)]
                                              [1 1] [(+ half√2) (+ half√2)]}))]
      (is (eps= 1 (perlin/noise perlin-data [0.25 0.25])))))
  (testing "noise extremes for the 3D noise"
    (let [inv√3 (/ (Math/sqrt 3))
          perlin-data (doto (perlin/create-perlin-data [2 2 2])
                        (set-3D-grid-values! {[0 0 0] [(- inv√3) (- inv√3) (- inv√3)]
                                              [1 0 0] [(+ inv√3) (- inv√3) (- inv√3)]
                                              [0 1 0] [(- inv√3) (+ inv√3) (- inv√3)]
                                              [1 1 0] [(+ inv√3) (+ inv√3) (- inv√3)]
                                              [0 0 1] [(- inv√3) (- inv√3) (+ inv√3)]
                                              [1 0 1] [(+ inv√3) (- inv√3) (+ inv√3)]
                                              [0 1 1] [(- inv√3) (+ inv√3) (+ inv√3)]
                                              [1 1 1] [(+ inv√3) (+ inv√3) (+ inv√3)]}))]
      (is (eps= 1 (perlin/noise perlin-data [0.25 0.25 0.25])))))
    (testing "noise extremes for the 4D noise"
    (let [half (/ 2)
          perlin-data (doto (perlin/create-perlin-data [2 2 2 2])
                        (set-4D-grid-values! {[0 0 0 0] [(- half) (- half) (- half) (- half)]
                                              [1 0 0 0] [(+ half) (- half) (- half) (- half)]
                                              [0 1 0 0] [(- half) (+ half) (- half) (- half)]
                                              [1 1 0 0] [(+ half) (+ half) (- half) (- half)]
                                              [0 0 1 0] [(- half) (- half) (+ half) (- half)]
                                              [1 0 1 0] [(+ half) (- half) (+ half) (- half)]
                                              [0 1 1 0] [(- half) (+ half) (+ half) (- half)]
                                              [1 1 1 0] [(+ half) (+ half) (+ half) (- half)]
                                              [0 0 0 1] [(- half) (- half) (- half) (+ half)]
                                              [1 0 0 1] [(+ half) (- half) (- half) (+ half)]
                                              [0 1 0 1] [(- half) (+ half) (- half) (+ half)]
                                              [1 1 0 1] [(+ half) (+ half) (- half) (+ half)]
                                              [0 0 1 1] [(- half) (- half) (+ half) (+ half)]
                                              [1 0 1 1] [(+ half) (- half) (+ half) (+ half)]
                                              [0 1 1 1] [(- half) (+ half) (+ half) (+ half)]
                                              [1 1 1 1] [(+ half) (+ half) (+ half) (+ half)]                                              }))]
      (is (eps= 1 (perlin/noise perlin-data [0.25 0.25 0.25 0.25]))))))
