(ns raytracer.utils-test
  (:require [clojure.test :refer :all]
            [raytracer.utils :as utils]))

(deftest test-quick-mmap
  (testing "works for empty literal vectors"
    (is (= []
           (utils/quick-map inc []))))
  (testing "behaves like vmap for vector literals"
    (is (= [1 2 3 4]
           (utils/quick-map inc [0 1 2 3]))))
  (testing "doesn't work for lists"
    (is (thrown? Exception  (utils/quick-map inc (list 0 1 2 3))))))

(deftest test-map-filter
  (testing "works for empty literal vectors"
    (is (= '()
           (utils/map-filter inc zero? []))))
  (testing "general case"
    (is (= [2 3 4]
           (utils/map-filter inc (complement zero?) [0 1 2 0 3 0 0]))))
  (testing "filter and transform functions are evaluated exactly once when all elements pass"
    (let [filter-counter (atom 0)
          true-with-counter (fn [x]
                              (swap! filter-counter inc)
                              true)          
          transform-counter (atom 0)
          inc-with-counter (fn [x]
                             (swap! transform-counter inc)
                             (inc x))]
      (is (= [1 2 3 4]
             (utils/map-filter inc-with-counter true-with-counter [0 1 2 3])))
      (is (= 4 @filter-counter))
      (is (= 4 @transform-counter))))
  (testing "filter function evaluated once and transform f never evaluated if no element passes"
    (let [filter-counter (atom 0)
          false-with-counter (fn [x]
                               (swap! filter-counter inc)
                               false)
          transform-counter (atom 0)
          inc-with-counter (fn [x]
                             (swap! transform-counter inc)
                             (inc x))]
      (is (= []
             (utils/map-filter inc-with-counter false-with-counter [0 1 2 3])))
      (is (= 4 @filter-counter))
      (is (= 0 @transform-counter))))
  (testing "forms passed as input are evaluated exactly once"
    (let [predicate (complement zero?)
          map-f inc
          counter (atom 0)
          generate-f (fn [x] (swap! counter inc) x)]
      (is (= [2]
             (utils/map-filter map-f predicate [(generate-f 0) (generate-f 1)])))
      (is (= 2 @counter)))))
