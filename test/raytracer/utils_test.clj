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
           (utils/map-filter inc (complement zero?) [0 1 2 0 3 0 0])))))
