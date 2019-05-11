(ns raytracer.pattern-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.point :as point]
            [raytracer.matrix :as matrix]
            [raytracer.transform :as transform]
            [raytracer.shapes :as shapes]
            [raytracer.pattern :as pattern]))

(def white [1 1 1])
(def black [0 0 0])

(deftest test-solid-pattern
  (testing "A solid pattern is the same everywhere"
    (let [solid (pattern/solid white)]
      (is (v= white ((:color-at solid) solid (point/point 0 0 0))))
      (is (v= white ((:color-at solid) solid (point/point 1.5 1.5 1.5))))
      (is (v= white ((:color-at solid) solid (point/point -0.5 0 0))))
      (is (v= white ((:color-at solid) solid (point/point -10 10 100)))))))

(deftest test-stripe-pattern
  (let [stripe (pattern/stripe white black)]
    (testing "Creating a stripe pattern"
      (is (= {:a white, :b black}
             (select-keys stripe [:a :b]))))
    (testing "A stripe pattern is constant in y"
      (is (v= white ((:color-at stripe) stripe (point/point 0 0 0))))
      (is (v= white ((:color-at stripe) stripe (point/point 0 1 0))))
      (is (v= white ((:color-at stripe) stripe (point/point 0 2 0)))))
    (testing "A stripe pattern is constant in z"
      (is (v= white ((:color-at stripe) stripe (point/point 0 0 0))))
      (is (v= white ((:color-at stripe) stripe (point/point 0 0 1))))
      (is (v= white ((:color-at stripe) stripe (point/point 0 0 2)))))
    (testing "A stripe pattern alternates in x"
      (is (v= white ((:color-at stripe) stripe (point/point 0 0 0))))
      (is (v= white ((:color-at stripe) stripe (point/point 0.9 0 0))))
      (is (v= black ((:color-at stripe) stripe (point/point 1 0 0)))))
    (is (v= black ((:color-at stripe) stripe (point/point -0.1 0 0))))
    (is (v= black ((:color-at stripe) stripe (point/point -1 0 0))))
    (is (v= white ((:color-at stripe) stripe (point/point -1.1 0 2))))))

(deftest test-gradient-pattern
  (let [gradient (pattern/gradient white black)]
    (testing "A gradient linearly interpolates between colors"
      (is (v= white ((:color-at gradient) gradient (point/point 0 0 0))))
      (is (v= [0.75 0.75 0.75] ((:color-at gradient) gradient (point/point 0.25 0 0))))
      (is (v= [0.5 0.5 0.5] ((:color-at gradient) gradient (point/point 0.5 0 0))))
      (is (v= [0.25 0.25 0.25] ((:color-at gradient) gradient (point/point 0.75 0 0)))))
    (testing "A gradient pattern has a default transformation"
      (is (v= matrix/identity-matrix (:transform gradient)))
      (is (v= matrix/identity-matrix (:inverse-transform gradient))))))

(deftest test-ring-pattern
  (let [ring (pattern/ring white black)]
    (testing "A ring should extend in both x and z"
      (is (v= white ((:color-at ring) ring (point/point 0 0 0))))
      (is (v= black ((:color-at ring) ring (point/point 1 0 0))))
      (is (v= black ((:color-at ring) ring (point/point 0 0 1))))
      (is (v= black ((:color-at ring) ring (point/point 0.708 0.708 0.708)))))))

(deftest test-checker-pattern
  (let [checker (pattern/checker white black)]
    (testing "Checker is not symmetric around 0"
      (is (not (v= ((:color-at checker) checker (point/point 0.5 0 0))
                   ((:color-at checker) checker (point/point -0.5 0 0)))))
      (is (not (v= ((:color-at checker) checker (point/point 0 0.5 0))
                   ((:color-at checker) checker (point/point 0 -0.5 0)))))
      (is (not (v= ((:color-at checker) checker (point/point 0 0 0.5))
                   ((:color-at checker) checker (point/point 0 0 -0.5))))))
    (testing "Checkers should repeat in x"
      (is (v= white ((:color-at checker) checker (point/point 0 0 0))))
      (is (v= white ((:color-at checker) checker (point/point 0.99 0 0))))
      (is (v= black ((:color-at checker) checker (point/point 1.01 0 0)))))  
    (testing "Checkers should repeat in y"
      (is (v= white ((:color-at checker) checker (point/point 0 0 0))))
      (is (v= white ((:color-at checker) checker (point/point 0 0.99 0))))
      (is (v= black ((:color-at checker) checker (point/point 0 1.01 0)))))
    (testing "Checkers should repeat in z"
      (is (v= white ((:color-at checker) checker (point/point 0 0 0))))
      (is (v= white ((:color-at checker) checker (point/point 0 0 0.99))))
      (is (v= black ((:color-at checker) checker (point/point 0 0 1.01)))))
    (testing "Checkers has x/y/z interactions"
      (is (v= white ((:color-at checker) checker (point/point 0 0 0))))
      (is (v= white ((:color-at checker) checker (point/point 0 1.5 1.5))))
      (is (v= white ((:color-at checker) checker (point/point 1.5 0.5 1.5))))
      (is (v= black ((:color-at checker) checker (point/point 1.5 1.5 1.5)))))))

(deftest test-nested-pattern
  (testing "Average blending"
    (let [blended (pattern/blend :average
                                 (pattern/checker white black)
                                 (pattern/checker black white))]
      (is (v= [0.5 0.5 0.5] ((:color-at blended) blended (point/point 0 0 0))))
      (is (v= [0.5 0.5 0.5] ((:color-at blended) blended (point/point 1.5 0 0))))
      (is (v= [0.5 0.5 0.5] ((:color-at blended) blended (point/point 1.5 1.5 0)))))))

(deftest test-perturb-pattern
  (testing "Perturbing a pattern changes the coordinates received by the pattern"
    (let [perturbed-pattern (pattern/perturb-pattern (pattern/checker white black)
                                                     (fn [point]
                                                       (point/point 1.1 0 0)))]
      (is (v= black ((:color-at perturbed-pattern) perturbed-pattern (point/point 0 0 0))))
      (is (v= black ((:color-at perturbed-pattern) perturbed-pattern (point/point 1.1 0 0))))
      (is (v= black ((:color-at perturbed-pattern) perturbed-pattern (point/point 1.1 1.1 0)))))
    (let [perturbed-pattern (pattern/perturb-pattern (pattern/checker white black)
                                                     (fn [point]
                                                       (point/point 1.1 1.1 0)))]
      (is (v= white ((:color-at perturbed-pattern) perturbed-pattern (point/point 0 0 0))))
      (is (v= white ((:color-at perturbed-pattern) perturbed-pattern (point/point 1.1 0 0))))
      (is (v= white ((:color-at perturbed-pattern) perturbed-pattern (point/point 1.1 1.1 0)))))))

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

(deftest test-test-pattern
  (testing "The pattern returns the point coordinate"
    (let [pattern (pattern/test)]
      (is (v= [0.2 0.3 0.4] (pattern/color-at-object pattern
                                                     (shapes/sphere)
                                                     (point/point 0.2 0.3 0.4)))))))
