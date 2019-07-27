(ns raytracer.shapes.group-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.const :as const]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]
            [raytracer.transform :as transform]
            [raytracer.ray :as ray]
            [raytracer.shapes :as shapes]
            [raytracer.shapes.shared :as shared]
            [raytracer.shapes.group :as group]))

(deftest test-group
  (testing "You can create an empty group with the identity matrix as transform"
    (let [group (group/group [])]
      (is (group/is-empty? group))
      (is (= group/empty-group group))
      (is (v= matrix/identity-matrix (:inverse-transform group)))
      (is (v= matrix/identity-matrix (:inverse-transposed-transform group))))))

(deftest test-local-intersect
  (testing "Intersection with empty group"
    (is (empty? (shared/local-intersect group/empty-group (ray/ray (point/point 0 0 0)
                                                                   (svector/svector 0 0 1))))))
  (testing "Intersection with a non-empty-group"
    (let [sphere1 (shapes/sphere)
          sphere2 (shapes/change-transform (shapes/sphere) (transform/translate 0 0 -3))
          sphere3 (shapes/change-transform (shapes/sphere) (transform/translate 5 0 0))
          group (group/group [sphere1 sphere2 sphere3])
          xs (shared/local-intersect group (ray/ray (point/point 0 0 -5)
                                                    (svector/svector 0 0 1)))]
      (is (= [sphere2 sphere2 sphere1 sphere1]
             (map :object xs)))))

  (testing "Intersecting a transformed group"
    (let [sphere (shapes/change-transform (shapes/sphere)
                                          (transform/translate 5 0 0))
          group (shapes/change-transform (group/group [sphere])
                                         (transform/scale 2 2 2))]
      (is (= 2 (count (shared/local-intersect group
                                              (ray/ray (point/point 10 0 -10)
                                                       (svector/svector 0 0 1)))))))))

