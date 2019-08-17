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

(deftest test-sort-infinite-shapes
  (let [sort-infinite-shapes #'raytracer.shapes.optimizers.bisection-recursive/sort-infinite-shapes]
    (testing "Only infinite shapes"
      (is (= {:finite []
              :infinite [(shapes/cylinder :maximum 100)
                         (shapes/cylinder :maximum 99)
                         (shapes/cylinder :maximum 98)]}
             (sort-infinite-shapes [(shapes/cylinder :maximum 100)
                                    (shapes/cylinder :maximum 99)
                                    (shapes/cylinder :maximum 98)]))))
    (testing "Only finite shapes"
      (is (= {:finite [(shapes/sphere)
                       (shapes/cube)
                       (shapes/cylinder :minimum 3 :maximum 100)]
              :infinite []}
             (sort-infinite-shapes [(shapes/sphere)
                                    (shapes/cube)
                                    (shapes/cylinder :minimum 3 :maximum 100)]))))
    (testing "Mixed shapes"
      (is (= {:finite [(shapes/sphere)
                       (shapes/cube)
                       (shapes/cylinder :minimum 3 :maximum 100)]
              :infinite [(shapes/cylinder :maximum 100)
                         (shapes/cylinder :maximum 99)
                         (shapes/cylinder :maximum 98)]}
             (sort-infinite-shapes [(shapes/sphere)
                                    (shapes/cylinder :maximum 100)
                                    (shapes/cube)
                                    (shapes/cylinder :minimum 3 :maximum 100)
                                    (shapes/cylinder :maximum 99)
                                    (shapes/cylinder :maximum 98)]))))))
