(ns raytracer.shapes.bounding-box-test
  (:require [clojure.test :refer :all]
            [raytracer.point :as point]
            [raytracer.ray :as ray]
            [raytracer.transform :as transform]
            [raytracer.shapes.bounding-box :as bounding-box]))

(deftest test-extremes-for-points
  (testing "Returns the unmodified point if there is only one"
    (is (= [(point/point 1 2 3) (point/point 1 2 3)]
           (bounding-box/extremes-from-points #{(point/point 1 2 3)}))))
  (testing "Returns the minimum and maximum values for all coordinates, respectively"
    (is (= [(point/point -1000 -100 1) (point/point 5 1000 3)]
           (bounding-box/extremes-from-points #{(point/point 1 2 3)
                                                (point/point -1000 1000 2)
                                                (point/point 5 -100 1)})))))

(deftest test-box-points-from-extremes
  (testing "Convert 2 points in a bounding box to a list of box vertices"
    (is (= #{(point/point 0, 0, 0)
             (point/point 10 0 0)
             (point/point 0 10 0)
             (point/point 10 10 0)
             (point/point 0 0 10)
             (point/point 10 0 10)
             (point/point 0 10 10)
             (point/point 10 10 10)}
           (bounding-box/box-points-from-extremes (point/point 0 0 0) (point/point 10 10 10))))))
