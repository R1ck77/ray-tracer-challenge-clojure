(ns raytracer.pattern-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.point :as point]
            [raytracer.pattern :as pattern]))

(def white [1 1 1])
(def black [0 0 0])

(deftest test-stripe-pattern
  (let [stripe (pattern/stripe white black)]
    (testing "Creating a stripe pattern"
           (is (= {:a white, :b black}
                  (select-keys stripe [:a :b]))))
    (testing "A stripe pattern is constant in y"
      (is (v= white ((:stripe-at stripe) stripe (point/point 0 0 0))))
      (is (v= white ((:stripe-at stripe) stripe (point/point 0 1 0))))
      (is (v= white ((:stripe-at stripe) stripe (point/point 0 2 0)))))
    (testing "A stripe pattern is constant in z"
      (is (v= white ((:stripe-at stripe) stripe (point/point 0 0 0))))
      (is (v= white ((:stripe-at stripe) stripe (point/point 0 0 1))))
      (is (v= white ((:stripe-at stripe) stripe (point/point 0 0 2)))))
    (testing "A stripe pattern alternates in x"
      (is (v= white ((:stripe-at stripe) stripe (point/point 0 0 0))))
      (is (v= white ((:stripe-at stripe) stripe (point/point 0.9 0 0))))
      (is (v= black ((:stripe-at stripe) stripe (point/point 1 0 0)))))
      (is (v= black ((:stripe-at stripe) stripe (point/point -0.1 0 0))))
      (is (v= black ((:stripe-at stripe) stripe (point/point -1 0 0))))
      (is (v= white ((:stripe-at stripe) stripe (point/point -1.1 0 2))))))


