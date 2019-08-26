(ns raytracer.shapes.placement-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :as tu]
            [raytracer.shapes.shared :as shared]
            [raytracer.shapes.placement :as placement]
            [raytracer.matrix :as matrix]
            [raytracer.transform :as transform]))

(def a-transform (transform/rotate-x 12.3 (transform/translate 1 2 3)))
(def inverse-of-a-transform (matrix/invert a-transform 4))

(deftest test-placement
  (testing "Placement without arguments"
    (let [placement (placement/placement)]
      (is (identical? (placement/get-transform placement)
             matrix/identity-matrix))
      (is (identical? (placement/get-inverse-transform placement)
             matrix/identity-matrix))
      (is (identical? (placement/get-inverse-transposed-transform placement)
             matrix/identity-matrix))))
  (testing "Placement with argument"
    (let [placement (placement/placement a-transform)]
      (is (identical? (placement/get-transform placement)
             a-transform))
      (is (tu/v= (placement/get-inverse-transform placement)
                 inverse-of-a-transform))
      (is (tu/v= (placement/get-inverse-transposed-transform placement)
                 (matrix/transpose inverse-of-a-transform))))))

(deftest test-change-transform
  (testing "Changing transform without changing matrix"
    (let [placement (placement/placement a-transform)]
      (is (identical? placement
                      (shared/change-transform placement a-transform))))))
