(ns raytracer.shapes.bounding-box-test
  (:require [clojure.test :refer :all]
            [raytracer.const :as const]
            [raytracer.point :as point]
            [raytracer.tuple :as tuple]
            [raytracer.ray :as ray]
            [raytracer.transform :as transform]
            [raytracer.shapes.bounding-box :as bounding-box]))

(def small-value (/ const/EPSILON 100))

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

(deftest test-almost-identical
  (testing "Returns true for points where each coordinate is different by less than const/EPSILON"
    (is (bounding-box/almost-identical (tuple/tuple (+ 1 small-value) (+ 2 small-value) 3 4) (tuple/tuple (- 1 small-value) 2 (+ 3 small-value) (+ 4 small-value))))
    (is (bounding-box/almost-identical (tuple/tuple 1.0 2.0 3.0 4.0) (tuple/tuple 1 2 3 4))))
  (testing "Returns false if any coordinate differs for EPSILON or more"
    (let [delta (* const/EPSILON 1.1)]
      (is (not (bounding-box/almost-identical (tuple/tuple delta 2 3 12) (tuple/tuple 0 2 3 12))))
      (is (not (bounding-box/almost-identical (tuple/tuple 0 (+ 2 delta) 3 12) (tuple/tuple 0 2 3 12))))
      (is (not (bounding-box/almost-identical (tuple/tuple 0 2 (+ 3 delta) 12) (tuple/tuple 0 2 3 12))))
      (is (not (bounding-box/almost-identical (tuple/tuple 0 2 3 (+ 12 delta)) (tuple/tuple 0 2 3 12))))
      (is (not (bounding-box/almost-identical (tuple/tuple 0 2 3 12) (tuple/tuple delta 2 3 12))))
      (is (not (bounding-box/almost-identical (tuple/tuple 0 2 3 12) (tuple/tuple 0 (+ 2 delta) 3 12))))
      (is (not (bounding-box/almost-identical (tuple/tuple 0 2 3 12) (tuple/tuple 0 2 (+ 3 delta) 12))))
      (is (not (bounding-box/almost-identical (tuple/tuple 0 2 3 12) (tuple/tuple 0 2 3 (+ 12 delta))))))))

(defn- same-extremes [[point-a-min point-a-max] [point-b-min point-b-max]]
  (and (bounding-box/almost-identical point-a-min point-b-min)
       (bounding-box/almost-identical point-b-max point-b-max)))

(deftest test-compute-filtered-transformed-extremes
  (testing "Returns an empty collection if the input points are almost identical"
    (let [extremes [(point/point small-value (- small-value) small-value)
                    (point/point 0 0.0 0.0)]
          transform (transform/translate 10 20 30 (transform/rotate-x const/half𝛑))]
      (is (empty? (bounding-box/compute-filtered-transformed-extremes extremes transform)))))
  (testing "Returns the same points (within error) if the transform is the identity matrix"
    (let [extremes [(point/point 1 2 3)
                    (point/point 5 6 7)]]
      (is (= '(true true)
             (map bounding-box/almost-identical
                  extremes
                  (bounding-box/compute-filtered-transformed-extremes extremes (transform/rotate-x (- const/half𝛑) (transform/rotate-x const/half𝛑))))))))
  (testing "Returns a properly rotated cube if you turn the cube by 45 degrees"
    (let [extremes [(point/point -2 -2 -2)
                    (point/point 2 2 2)]
          diagonal (* 2 (Math/sqrt 2))
          expected [(point/point (- diagonal) -2 (- diagonal))
                    (point/point diagonal 2 diagonal)]]
      
      (is (same-extremes expected
                         (bounding-box/compute-filtered-transformed-extremes extremes
                                                                             (transform/rotate-y const/quarter𝛑))))))
  (testing "Returns a an empty collection if the input points are collapsed to a single point"
    (let [extremes [(point/point -10 -10 -10)
                    (point/point 3 3 3)]]      
      (is (empty? (bounding-box/compute-filtered-transformed-extremes extremes (transform/scale 0 0 0)))))))
