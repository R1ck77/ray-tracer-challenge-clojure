(ns raytracer.grouping.base-zipper-test
  (:require [clojure.test :refer :all]
            [clojure.zip :as zip]
            [raytracer.shapes.group :as group]
            [raytracer.grouping.base-zipper :as bz]
            [raytracer.grouping.shared :as shared]
            [raytracer.shapes :as shapes]))

(def empty-group (assoc (shapes/group []) :name :empty-group))
(def shape-a (assoc (shapes/sphere) :name :shape-a))
(def shape-b (assoc (shapes/sphere) :name :shape-b))
(def shape-c (assoc (shapes/sphere) :name :shape-c))
(def shape-d (assoc (shapes/sphere) :name :shape-d))
(def group-a (assoc (shapes/group [shape-a]) :name :group-a))
(def group-b (assoc (shapes/group [shape-b shape-c]) :name :group-b))
(def group-c (assoc (shapes/group [group-a group-b shape-d]) :name :group-c))
(def group-0 (assoc (shapes/group [group-a shape-a group-b group-c]) :name :group-0))

(def a-group-zipper (bz/new-group-zipper group-0))
(def a-singleton-zipper (bz/new-group-zipper shape-a))

(deftest test-new-group-zipper
  (testing "branch? (root)"
    (is (zip/branch? a-group-zipper))
    (is (not (zip/branch? a-singleton-zipper))))
  (testing "get-children (root)"
    (is (= [group-a shape-a group-b group-c]
           (zip/children a-group-zipper))))
  (testing "remove node"
    (is (= [shape-a group-b group-c]
           (zip/children
            (zip/remove
             (zip/down a-group-zipper)))))))

(deftest test-traverse
  (testing "Traverse returns the root object"
    (is (= [shape-a] (bz/traverse (#'bz/new-group-zipper shape-a))))
    (is (= [empty-group] (bz/traverse (#'bz/new-group-zipper empty-group)))))
  (testing "Traverse returns nested objects"
    (is (= #{:shape-a :shape-b :shape-c :shape-d :group-a :group-b :group-c :group-0}
           (into #{} (map :name (bz/traverse (#'bz/new-group-zipper group-0))))))))

(deftest test-get-all-matching-objects
  (testing "Returns the group object"
    (let [group (group/group [])]
      (is (= #{group}
             (bz/get-all-matching-objects (#'bz/new-group-zipper (group/group []))
                                              (constantly true))))))
  (testing "Returns nested objects"
    (let [group (assoc (group/group [shape-a]) :name :group-a)]
      (is (= #{group shape-a}
             (bz/get-all-matching-objects (#'bz/new-group-zipper group)
                                              (constantly true)))))))

(deftest test-update-objects
  (testing "Updates all objects, group objects included"
    (let [root (group/group [(shapes/sphere)
                             (shapes/sphere)
                             (group/group [])
                             (shapes/cube)
                             (group/group [(group/group [(shapes/cone)
                                                         (shapes/cube)
                                                         (shapes/sphere)])
                                           (shapes/cube)
                                           (shapes/sphere)])])
          zipper (bz/new-group-zipper root)
          counter (ref 0)
          names (ref #{})]
      (let [updated-zipper (bz/update-zipper zipper (fn [node]
                                                      (dosync
                                                       (let [new-name (str "name-" @counter)]
                                                         (alter counter inc)
                                                         (alter names #(conj % new-name))
                                                         (assoc node :name new-name)))))
            new-objects (bz/get-all-matching-objects (bz/new-group-zipper updated-zipper)
                                                     (constantly true))]
        (is (= @names
               (into #{} (map :name new-objects))))))))
