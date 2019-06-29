(ns raytracer.grouping.caching-hierarchy-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.const :as const]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.transform :as transform]
            [raytracer.shapes :as shapes]
            [raytracer.shapes.group :as group]            
            [raytracer.shapes.shared :as shared]
            [raytracer.grouping.hierarchy :as hierarchy]
            [raytracer.grouping.caching-hierarchy :as caching-hierarchy]
            [raytracer.grouping.shared :as grouping-shared]
            [clojure.zip :as zip]))

(deftest test-to-world-coordinates
  (testing "Converting a point from world to object space"
    (let [sphere (shapes/change-transform (shapes/sphere) (transform/translate 5 0 0))
          group2 (shapes/change-transform (group/group [sphere]) (transform/scale 2 2 2))
          group1 (shapes/change-transform (group/group [group2]) (transform/rotate-y const/half𝛑))
          hierarchy (caching-hierarchy/caching-hierarchy group1)]
      (is (t= (point/point 0 0 -1)
              (grouping-shared/world-to-local-coordinates hierarchy sphere (point/point -2 0 -10))))))
  (testing "Converting a normal from object to world space"
    (let [sphere (shapes/change-transform (shapes/sphere) (transform/translate 5 0 0))
          group2 (shapes/change-transform (group/group [sphere]) (transform/scale 1 2 3))
          group1 (shapes/change-transform (group/group [group2]) (transform/rotate-y const/half𝛑))
          hierarchy (caching-hierarchy/caching-hierarchy group1)
          third√3 (/ (Math/sqrt 3) 3)]
      (is (t= (svector/svector 0.2857, 0.4286, -0.8571)
              (grouping-shared/local-to-world-coordinates hierarchy sphere (svector/svector third√3 third√3 third√3))))))
  (testing "Converting a normal from object to world space: regression to check optimization"
    (let [sphere (shapes/change-transform (shapes/sphere) (transform/translate 5 0 0))
          group4 (shapes/change-transform (group/group [sphere]) (transform/scale 1 2 3))
          group3 (shapes/change-transform (group/group [group4]) (transform/rotate-x 0.834))
          group2 (shapes/change-transform (group/group [group3]) (transform/translate 3 2 1.2))
          group1 (shapes/change-transform (group/group [group2]) (transform/rotate-y 0.23))
          hierarchy (caching-hierarchy/caching-hierarchy group1)
          third√3 (/ (Math/sqrt 3) 3)]
      (is (t= (svector/svector 0.9507001 0.076358 0.300564)
              (grouping-shared/local-to-world-coordinates hierarchy sphere (svector/svector third√3 third√3 third√3))))))  )
