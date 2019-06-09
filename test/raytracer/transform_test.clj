(ns raytracer.transform-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.const :as const]
            [raytracer.transform :as transform]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]))

(deftest test-translate
  (testing "translation"
    (is (t= (point/point 7 -2 9)
            (matrix/transform (transform/translate 5 -3 2 )
                              (point/point 2 1 7 )))))
  (testing "inverse of a translation"
    (is (t= (point/point -8 7 3)
            (matrix/transform (matrix/invert (transform/translate 5 -3 2) 4)
                              (point/point -3 4 5)))))
  (testing "translation on vector"
    (is (t= (svector/svector -3 4 5)
            (matrix/transform (transform/translate 5 -3 2)
                              (svector/svector -3 4 5))))))

(deftest test-scale
  (testing "scaling"
    (is (t= (point/point -8 18 32)
            (matrix/transform (transform/scale 2 3 4)
                              (point/point -4 6 8)))))
  (testing "inverse of a scaling"
    (is (t= (svector/svector -2 2 2)
            (matrix/transform (matrix/invert (transform/scale 2 3 4) 4)
                              (svector/svector -4 6 8)))))
  (testing "scaling on vector"
    (is (t= (svector/svector -8 18 32)
            (matrix/transform (transform/scale 2 3 4)
                              (svector/svector -4 6 8)))))
  (testing "scaling by negative index"
    (is (t= (point/point -2 3 4)
            (matrix/transform (transform/scale -1 1 1)
                              (point/point 2 3 4))))))

(deftest test-rotation-x
  (testing "various x axis rotation"
    (is (t= (point/point 0 const/half√2 const/half√2)
            (matrix/transform (transform/rotate-x (/ Math/PI 4))
                              (point/point 0 1 0))))
    (is (t= (point/point 0 0 1)
            (matrix/transform (transform/rotate-x (/ Math/PI 2))
                              (point/point 0 1 0)))))
  (testing "inverse of various x axis rotation"
    (is (t= (point/point 0 1 0)
            (matrix/transform (matrix/invert (transform/rotate-x (/ Math/PI 4)) 4)
                              (point/point 0 const/half√2 const/half√2))))
    (is (t= (point/point 0 1 0)
            (matrix/transform (matrix/invert (transform/rotate-x (/ Math/PI 2)) 4)
                              (point/point 0 0 1))))))

(deftest test-rotation-y
  (testing "various y axis rotation"
    (is (t= (point/point const/half√2 0 const/half√2)
            (matrix/transform (transform/rotate-y (/ Math/PI 4))
                              (point/point 0 0 1))))
    (is (t= (point/point 1 0 0)
            (matrix/transform (transform/rotate-y (/ Math/PI 2))
                              (point/point 0 0 1)))))
  (testing "inverse of various y axis rotation"
    (is (t= (point/point 0 0 1)
            (matrix/transform (matrix/invert (transform/rotate-y (/ Math/PI 4)) 4)
                              (point/point const/half√2 0 const/half√2))))
    (is (t= (point/point 0 0 1)
            (matrix/transform (matrix/invert (transform/rotate-y (/ Math/PI 2)) 4)
                              (point/point 1 0 0))))))

(deftest test-rotation-z
  (testing "various z axis rotation"
    (is (t= (point/point (- const/half√2) const/half√2 0)
            (matrix/transform (transform/rotate-z (/ Math/PI 4))
                              (point/point 0 1 0))))
    (is (t= (point/point -1 0 0)
            (matrix/transform (transform/rotate-z (/ Math/PI 2))
                              (point/point 0 1 0)))))
  (testing "inverse of various z axis rotation"
    (is (t= (point/point 0 1 0)
            (matrix/transform (matrix/invert (transform/rotate-z (/ Math/PI 4)) 4)
                              (point/point (- const/half√2) const/half√2 0))))
    (is (t= (point/point 0 1 0)
            (matrix/transform (matrix/invert (transform/rotate-z (/ Math/PI 2)) 4)
                              (point/point -1 0 0))))))

(defmacro test-one-shearing [xy xz yx yz zx zy point-in result]
  `(is (t= (apply point/point ~result)
           (matrix/transform (transform/shear ~xy ~xz ~yx ~yz ~zx ~zy)
                             (apply point/point ~point-in)))))

(deftest test-shearing
  (testing "single component shearings"
    (test-one-shearing 1 0 0 0 0 0 [2 3 4] [5 3 4])
    (test-one-shearing 0 1 0 0 0 0 [2 3 4] [6 3 4])
    (test-one-shearing 0 0 1 0 0 0 [2 3 4] [2 5 4])
    (test-one-shearing 0 0 0 1 0 0 [2 3 4] [2 7 4])
    (test-one-shearing 0 0 0 0 1 0 [2 3 4] [2 3 6])
    (test-one-shearing 0 0 0 0 0 1 [2 3 4] [2 3 7])))

(deftest test-chaining
  (testing "transforms chaining"
    (let [translation (transform/translate 10 5 7)
          scaling (transform/scale 5 5 5)
          rotation (transform/rotate-x (/ Math/PI 2))]
      (is (t= (matrix/transform translation
                                (matrix/transform scaling
                                                  (matrix/transform rotation
                                                                    (point/point 1 0 1))))
              (matrix/transform (transform/translate 10 5 7
                                                     (transform/scale 5 5 5
                                                                      (transform/rotate-x (/ Math/PI 2))))
                                (point/point 1 0 1)))))))
