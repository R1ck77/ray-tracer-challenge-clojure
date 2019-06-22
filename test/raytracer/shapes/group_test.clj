(ns raytracer.shapes.group-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.matrix :as matrix]
            [raytracer.shapes.group :as group]))

(deftest test-group
  (testing "You can create a group with the identity matrix as transform"
    (let [group (group/group)]
      (is (= (group/group) group))
      (is (v= matrix/identity-matrix (:inverse-transform group)))
      (is (v= matrix/identity-matrix (:inverse-transpose-transform group))))))
