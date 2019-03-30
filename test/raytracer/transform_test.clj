(ns raytracer.transform-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.transform :as transform]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]))

(deftest test-translate
  (testing "translation"
    (is (eps4= (point/point 7 -2 9)
               (matrix/transform (transform/translation 5 -3 2 )
                                 (point/point 2 1 7 )))))
  (testing "inverse of a translation"
    (is (eps4= (point/point -8 7 3)
               (matrix/transform (matrix/invert (transform/translation 5 -3 2) 4)
                                 (point/point -3 4 5)))))
  (testing "translation on vector"
    (is (eps4= (svector/svector -3 4 5)
               (matrix/transform (transform/translation 5 -3 2)
                                 (svector/svector -3 4 5))))))

(deftest test-scale
  (testing "scaling"
    (is (eps4= (point/point -8 18 32)
               (matrix/transform (transform/scaling 2 3 4)
                                 (point/point -4 6 8)))))
  (testing "inverse of a scaling"
    (is (eps4= (svector/svector -2 2 2)
               (matrix/transform (matrix/invert (transform/scaling 2 3 4) 4)
                                 (svector/svector -4 6 8)))))
  (testing "scaling on vector"
    (is (eps4= (svector/svector -8 18 32)
               (matrix/transform (transform/scaling 2 3 4)
                                 (svector/svector -4 6 8)))))
    (testing "scaling by negative index"
    (is (eps4= (point/point -2 3 4)
               (matrix/transform (transform/scaling -1 1 1)
                                 (point/point 2 3 4))))))

