(ns raytracer.shapes.optimizers.bisection-recursive-test
  (:require [clojure.test :refer :all]
            [raytracer.shapes.optimizers.bisection-recursive :as br]
            [raytracer.shapes :as shapes]
            [raytracer.shapes.shared :as shared]
            [raytracer.transform :as transform]))


(deftest test-is-infinite?
  (testing "a cylinder is considered infinite if it extends to infinity on any side"
    (is (not (br/is-infinite? (shapes/cylinder :minimum -100, :maximum 100))))
    (is (br/is-infinite? (shapes/cylinder :maximum 100)))
    (is (br/is-infinite? (shapes/cylinder :minimum -100)))
    (is (br/is-infinite? (shapes/cylinder))))
  (testing "a shape is infinite if scaled to infinity in any direction"
    (is (not (br/is-infinite? (shapes/sphere))))
    (is (br/is-infinite? (shared/transform (shapes/sphere)
                                           (transform/scale Double/POSITIVE_INFINITY 0 0))))))
