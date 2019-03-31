(ns raytracer.transform-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.transform :as transform]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]))

(def half-√2 (/ (Math/sqrt 2) 2))

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

(deftest test-rotation-x
  (testing "various x axis rotation"
    (is (eps4= (point/point 0 half-√2 half-√2)
               (matrix/transform (transform/rotation-x (/ Math/PI 4))
                                 (point/point 0 1 0))))
    (is (eps4= (point/point 0 0 1)
               (matrix/transform (transform/rotation-x (/ Math/PI 2))
                                 (point/point 0 1 0)))))
  (testing "inverse of various x axis rotation"
    (is (eps4= (point/point 0 1 0)
               (matrix/transform (matrix/invert (transform/rotation-x (/ Math/PI 4)) 4)
                                 (point/point 0 half-√2 half-√2))))
    (is (eps4= (point/point 0 1 0)
               (matrix/transform (matrix/invert (transform/rotation-x (/ Math/PI 2)) 4)
                                 (point/point 0 0 1))))))

(deftest test-rotation-y
  (testing "various y axis rotation"
    (is (eps4= (point/point half-√2 0 half-√2)
               (matrix/transform (transform/rotation-y (/ Math/PI 4))
                                 (point/point 0 0 1))))
    (is (eps4= (point/point 1 0 0)
               (matrix/transform (transform/rotation-y (/ Math/PI 2))
                                 (point/point 0 0 1)))))
  (testing "inverse of various y axis rotation"
    (is (eps4= (point/point 0 0 1)
               (matrix/transform (matrix/invert (transform/rotation-y (/ Math/PI 4)) 4)
                                 (point/point half-√2 0 half-√2))))
    (is (eps4= (point/point 0 0 1)
               (matrix/transform (matrix/invert (transform/rotation-y (/ Math/PI 2)) 4)
                                 (point/point 1 0 0))))))

(deftest test-rotation-z
  (testing "various z axis rotation"
    (is (eps4= (point/point (- half-√2) half-√2 0)
               (matrix/transform (transform/rotation-z (/ Math/PI 4))
                                 (point/point 0 1 0))))
    (is (eps4= (point/point -1 0 0)
               (matrix/transform (transform/rotation-z (/ Math/PI 2))
                                 (point/point 0 1 0)))))
  (testing "inverse of various z axis rotation"
    (is (eps4= (point/point 0 1 0)
               (matrix/transform (matrix/invert (transform/rotation-z (/ Math/PI 4)) 4)
                                 (point/point (- half-√2) half-√2 0))))
    (is (eps4= (point/point 0 1 0)
               (matrix/transform (matrix/invert (transform/rotation-z (/ Math/PI 2)) 4)
                                 (point/point -1 0 0))))))

(defmacro test-one-shearing [xy xz yx yz zx zy point-in result]
  `(is (eps4= (apply point/point ~result)
              (matrix/transform (transform/shearing ~xy ~xz ~yx ~yz ~zx ~zy)
                                (apply point/point ~point-in)))))

(deftest test-shearing
  (testing "single component shearings"
    (test-one-shearing 1 0 0 0 0 0 [2 3 4] [5 3 4])
    (test-one-shearing 0 1 0 0 0 0 [2 3 4] [6 3 4])
    (test-one-shearing 0 0 1 0 0 0 [2 3 4] [2 5 4])
    (test-one-shearing 0 0 0 1 0 0 [2 3 4] [2 7 4])
    (test-one-shearing 0 0 0 0 1 0 [2 3 4] [2 3 6])
    (test-one-shearing 0 0 0 0 0 1 [2 3 4] [2 3 7])))
