(ns raytracer.svector-test
  (:require [clojure.test :refer :all]
            [raytracer.svector :refer :all]
            [raytracer.point :as point]
            [raytracer.test-utils :refer [eps= v=]]))

;;; TODO/FIXME all tests comparing with [] are implementation dependent!!!!
;;; TODO/FIXME some tests are not on real vectors!!!

(def inv-sqrt-3 (/ (Math/sqrt 3)))

(deftest test-vector
  (testing "svector creates a tuple with w == 0"
    (is (svector? (svector 1 2 3)))
    (is (not (point/point? (svector 4 1 2))))))

(deftest test-svector?
  (testing "svector? returns true for tuple when w is 0"
    (is (svector? [3.5 1.2 5.4 0.0]))
    (is (svector? [32 12.0 -19.0 1e-7]))
    (is (svector? [3.5 1.2 5.4 -1e-7])))
  (testing "svector? returns false for tuple with w not 0"
    (is (not (svector? [3.5 1.2 5.4 1])))
    (is (not (svector? [32 12.0 -19.0 0.001])))
    (is (not (svector? [3.5 1.2 5.4 0.001])))))

(deftest test-mul
  (testing "multiplying a tuple by a scalar"
    (v= [3.5 -7 10.5 -14] (mul [1 -2 3 -4] 3.5)))
  (testing "multiplying a tuple by a fraction"
    (v= [0.5 -1 1.5 -2] (mul [1 -2 3 -4] 0.5))))

(deftest test-neg
  (testing "negating a vector"
    (v= (svector -1 2 3) (neg (svector 1 -2 3)))))


(defmacro do-test-mag [expected v]
  `(testing ~(str "computing the magnitude of " v)
     (is (eps= ~expected (mag ~(apply svector v))))))

(deftest test-mag
  (do-test-mag 1 [1 0 0])
  (do-test-mag 1 [0 1 0])
  (do-test-mag 1 [0 0 1])
  (do-test-mag (Math/sqrt 14) [1 2 3])
  (do-test-mag (Math/sqrt 14) [-1 -2 -3]))

(defmacro do-test-normalize [expected v]
  `(testing ~(str "normalizing " v)
     (let [res# (normalize (apply svector ~v))]
       (is (v= (apply svector ~expected) res#))
       (is (eps= 1 (mag res#))))))

(deftest test-normalize
  (do-test-normalize [1 0 0] [4 0 0])
  (do-test-normalize [inv-sqrt-3 inv-sqrt-3 inv-sqrt-3] [4 4 4])
  (do-test-normalize [(/ (Math/sqrt 14)) (/ 2 (Math/sqrt 14)) (/ 3 (Math/sqrt 14))]
                [1 2 3]))

(deftest test-dot
  (is (eps= 20
            (dot (svector 1 2 3)
                 (svector 2 3 4)))))

(deftest test-cross
  (is (v= (svector -1 2 -1)
          (cross (svector 1 2 3) (svector 2 3 4))))
  (is (v= (svector 1 -2 1)
          (cross (svector 2 3 4) (svector 1 2 3))))
  (is (eps= 0 (dot (svector -34 54 124)
                   (cross (svector -34 54 124) (svector 34 12 1))))))
