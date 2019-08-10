(ns raytracer.wavefront.parser-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :as tu]
            [raytracer.point :as point]
            [raytracer.wavefront.parser :as parser]))


(deftest test-parser
  (testing "Ignoring unrecognized lines"
    (let [result (parser/parse "There was a young lady named Bright
who traveled much faster than light.
She set out one day
in a relative way,
and came back the previous night.")]
      (is (= 5 (:ignored result)))))
  (testing "Vertex records"
    (let [result (parser/parse "v -1 1 0
v -1.0000 0.5000 0.0000
v 1 0 0
v 1 1 0")]
      (is (tu/t= (point/point -1 1 0)
                 (get (:vertices result) 1)))
      (is (tu/t= (point/point -1 0.5 0)
                 (get (:vertices result) 2)))
      (is (tu/t= (point/point 1 0 0)
                 (get (:vertices result) 3)))
      (is (tu/t= (point/point 1 1 0)
                 (get (:vertices result) 4))))))
