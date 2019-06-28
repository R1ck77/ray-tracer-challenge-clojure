(ns raytracer.hierarchy-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.const :as const]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.transform :as transform]
            [raytracer.shapes :as shapes]
            [raytracer.shapes.group :as group]            
            [raytracer.shapes.shared :as shared]
            [raytracer.hierarchy :as hierarchy]
            [clojure.zip :as zip]))

(deftest test-to-world-coordinates
  (testing "Converting a point from world to object space"
    (let [sphere (shapes/change-transform (shapes/sphere) (transform/translate 5 0 0))
          group2 (shapes/change-transform (group/group [sphere]) (transform/scale 2 2 2))
          group1 (shapes/change-transform (group/group [group2]) (transform/rotate-y const/half𝛑))
          hierarchy (hierarchy/hierarchy group1)]
      (is (t= (point/point 0 0 -1)
              (hierarchy/world-to-local-coordinates hierarchy sphere (point/point -2 0 -10))))))
  (testing "Converting a normal from object to world space"
    (let [sphere (shapes/change-transform (shapes/sphere) (transform/translate 5 0 0))
          group2 (shapes/change-transform (group/group [sphere]) (transform/scale 1 2 3))
          group1 (shapes/change-transform (group/group [group2]) (transform/rotate-y const/half𝛑))
          hierarchy (hierarchy/hierarchy group1)
          third√3 (/ (Math/sqrt 3) 3)]
      (is (t= (svector/svector 0.2857, 0.4286, -0.8571)
              (hierarchy/local-to-world-coordinates hierarchy sphere (svector/svector third√3 third√3 third√3)))))))
