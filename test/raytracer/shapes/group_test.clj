(ns raytracer.shapes.group-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.matrix :as matrix]
            [raytracer.shapes.group :as group]
            [raytracer.shapes.sphere :as sphere]
            [raytracer.shapes.shared :as shared]))

(deftest test-group
  (testing "Groups are created empty"
    (is (empty? (:shapes (group/group)))))
  (testing "Groups start with no transformation"
    (is (v= matrix/identity-matrix (:transform (group/group))))))

(deftest test-add
  (testing "Adding a child to a group"
    (let [empty-group (group/group)
          sphere (sphere/sphere)
          association (shared/add empty-group sphere)]
      ;;; HIC SUNT LEONES (AND MUTABILITY)
      )
    



When 
add_child(g, s) 

Then 
g is not empty 

And 
g includes s 

And 
s.parent = g
    
    ))
