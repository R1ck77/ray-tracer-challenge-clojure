(ns raytracer.shapes.bounding-box-test
  (:require [clojure.test :refer :all]
            [raytracer.shapes.bounding-box :as bounding-box]))

(deftest test-extremes-for-points
  (testing "Returns the unmodified point if there is only one"
    (is (= [[1 2 3] [1 2 3]]
           (bounding-box/extremes-from-points #{[1 2 3]}))))
  (testing "Returns the minimum and maximum values for all coordinates, respectively"
    (is (= [[-1000 -100 1] [5 1000 3]]
           (bounding-box/extremes-from-points #{[1 2 3]
                                                [-1000 1000 2]
                                                [5 -100 1]})))))
