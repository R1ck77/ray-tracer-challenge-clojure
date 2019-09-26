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
            [raytracer.shapes.group :as group]
            [raytracer.shapes.placement :as placement]
            [raytracer.shapes.parent :as parent]
            [raytracer.grouping.hierarchy :as hierarchy]))

(deftest test-group
  (testing "You can create an empty group with the identity matrix as transform"
    (let [group (group/group [])]
      (is (parent/is-empty? group))
      (is (= group/empty-group group))
      (is (v= matrix/identity-matrix
              (-> group :placement placement/get-inverse-transform)))
      (is (v= matrix/identity-matrix
              (-> group :placement placement/get-inverse-transposed-transform))))))

(deftest test-local-intersect
  (testing "Intersection with empty group"
    (is (empty? (shared/local-intersect group/empty-group (ray/ray (point/point 0 0 0)
                                                                   (svector/svector 0 0 1))))))
  (testing "Intersection with a non-empty-group"
    (let [sphere1 (shapes/sphere)
          sphere2 (shared/change-transform (shapes/sphere) (transform/translate 0 0 -3))
          sphere3 (shared/change-transform (shapes/sphere) (transform/translate 5 0 0))
          group (group/group [sphere1 sphere2 sphere3])
          xs (shared/local-intersect group
                                     (ray/ray (point/point 0 0 -5)
                                              (svector/svector 0 0 1)))]
      (is (= [sphere2 sphere2 sphere1 sphere1]
             (map :object xs)))))

  (testing "Intersecting a transformed group (bounding box ignored)"
    (with-redefs [ray/*use-bounding-boxes* false]
      (let [sphere (shared/change-transform (shapes/sphere)
                                     (transform/translate 5 0 0))
            group (shared/change-transform (group/group [sphere])
                                    (transform/scale 2 2 2))]
        (is (= 2 (count (ray/intersect (ray/ray (point/point 10 0 -10)
                                                (svector/svector 0 0 1))
                                       group))))))))

(deftest test-includes?
  (let [empty-group (group/group [])]
    (testing "An empty group includes itself"
      (is (shared/includes? empty-group empty-group)))
    (testing "An empty group does not include a copy of itself"
      (is (not (identical? (group/group []) (group/group []))))
      (is (not (shared/includes? empty-group (group/group [])))))
    (testing "An empty group does not include a different object"
      (is (not (shared/includes? empty-group (shapes/cube))))))
  (let [cube (shapes/cube)
        cylinder (shapes/cylinder)
        cone (shapes/cone)
        sphere1 (shapes/sphere)
        sphere2 (shapes/sphere)
        sub-group (shapes/group [sphere1 cone])
        group (group/group [cube cylinder sub-group sphere2])]
    (testing "A group includes itself"
      (is (shared/includes? group group)))
    (testing "A group includes all sub-elements"
      (is (shared/includes? group cube))
      (is (shared/includes? group cylinder))
      (is (shared/includes? group cone))
      (is (shared/includes? group sphere1))
      (is (shared/includes? group sphere2))
      (is (shared/includes? group sub-group)))
    (testing "A group doesn't include a different one because it's equal"
      (is (not (shared/includes? sub-group (shapes/group [sphere1 cone])))))))
