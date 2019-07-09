(ns raytracer.grouping.zipper-test
  (:require [clojure.test :refer :all]
            [clojure.zip :as zip]
            [raytracer.shapes.group :as group]
            [raytracer.grouping.zipper :as zipper]
            [raytracer.grouping.shared :as shared]))

(deftest testing-get-root
  (testing "Returns the group at the root of the hieararchy"
    (let [root (group/group [:a :b :c])]
      (is (= root (shared/get-root (zipper/create-zipper root)))))))

(deftest testing-get-all-objects
  (testing "Returns an empty collection if the root group is empty"
    (is (empty? (shared/get-all-objects
                 (zipper/create-zipper
                  (group/group []))))))
  (testing "Returns all objects in a flat hierarchy"
    (is (= #{:a, :c, :b, :d}
           (shared/get-all-objects
            (zipper/create-zipper
             (group/group [:a :c :b :d]))))))  
  (testing "Returns objects in nested groups as well"
    (is (= #{:a, :c, :b, :d}
           (shared/get-all-objects
            (zipper/create-zipper
             (group/group [:a :c (group/group [:b (group/group [:d (group/group [])])])]))))))
  (testing "Returns only the objects, not the groups"
        (is (= #{}
           (shared/get-all-objects
            (zipper/create-zipper
             (group/group [(group/group [(group/group [])])])))))))


