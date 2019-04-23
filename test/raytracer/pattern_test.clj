(ns raytracer.pattern-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.point :as point]
            [raytracer.transform :as transform]
            [raytracer.shapes :as shapes]
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

(deftest test-color-at-object
  (testing "Stripes with an object transformation"
    (let [object (shapes/change-transform (shapes/sphere)
                                          (transform/scale 2 2 2))
          pattern (pattern/stripe white black)]
      (is (v= white (pattern/color-at-object pattern
                                             object
                                             (point/point 1.5 0 0))))))
  (testing "Stripes with a pattern transformation"
    (let [object (shapes/sphere)
          pattern (pattern/change-transform (pattern/stripe white black)
                                            (transform/scale 2 2 2))]
      (is (v= white (pattern/color-at-object pattern
                                             object
                                             (point/point 1.5 0 0))))))
  (testing "Stripes with both an object and a pattern transformation"
    (let [object (shapes/change-transform (shapes/sphere)
                                          (transform/scale 2 2 2))
          pattern (pattern/change-transform (pattern/stripe white black)
                                            (transform/translate 0.5 0 0))]
      (is (v= white (pattern/color-at-object pattern
                                             object
                                             (point/point 2.5 0 0)))))))
