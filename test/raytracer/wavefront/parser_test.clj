(ns raytracer.wavefront.parser-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :as tu]
            [raytracer.point :as point]
            [raytracer.shapes.triangle :as triangle]
            [raytracer.wavefront.parser :as parser]))


(deftest test-parser
  (testing "Ignoring unrecognized lines"
    (let [result (parser/parse "There was a young lady named Bright
who traveled much faster than light.
She set out one day
in a relative way,
and came back the previous night.")]
      (is (= 5 (count (:ignored result))))))
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
                 (get (:vertices result) 4)))))
  (testing "Parsing triangle faces"
    (let [result (parser/parse "v -1 1 0
v -1 0 0
v 1 0 0
v 1 1 0
f 1 2 3
f 1 3 4")
          default-group (-> result :groups :default-group)]
      (is (= (triangle/triangle (point/point -1 1 0)
                                (point/point -1 0 0)
                                (point/point 1 0 0))
             (get default-group 1)))
      (is (= (triangle/triangle (point/point -1 1 0)
                                (point/point 1 0 0)
                                (point/point 1 1 0))
             (get default-group 2))))


    
    ))
