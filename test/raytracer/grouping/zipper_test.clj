(ns raytracer.grouping.zipper-test
  (:require [clojure.test :refer :all]
            [clojure.zip :as zip]
            [raytracer.shapes.group :as group]
            [raytracer.grouping.zipper :as zipper]
            [raytracer.grouping.shared :as shared]
            [raytracer.shapes :as shapes]))

(def shape-a (assoc (shapes/sphere) :name :shape-a))
(def shape-b (assoc (shapes/sphere) :name :shape-b))
(def shape-c (assoc (shapes/sphere) :name :shape-c))
(def shape-d (assoc (shapes/sphere) :name :shape-d))

(deftest testing-get-root
  (testing "Returns the group at the root of the hieararchy"
    (let [root (group/group [(shapes/sphere) (shapes/sphere) (shapes/sphere)])]
      (is (= root (shared/get-root (zipper/create-zipper root)))))))

(deftest testing-get-all-objects
  (testing "Returns an empty collection if the root group is empty"
    (is (empty? (shared/get-all-objects
                 (zipper/create-zipper
                  (group/group []))))))
  (testing "Returns all objects in a flat hierarchy"
    (is (= #{shape-a shape-b shape-c shape-d}
           (shared/get-all-objects
            (zipper/create-zipper
             (group/group [shape-a shape-b shape-c shape-d]))))))  
  (testing "Returns objects in nested groups as well"
    (is (= #{shape-a shape-b shape-c shape-d}
           (shared/get-all-objects
            (zipper/create-zipper
             (group/group [shape-a shape-c (group/group [shape-b (group/group [shape-d (group/group [])])])]))))))
  (testing "Returns only the objects, not the groups"
        (is (= #{}
           (shared/get-all-objects
            (zipper/create-zipper
             (group/group [(group/group [(group/group [])])])))))))

(deftest testing-get-all-matching-objects
  (testing "Returns the group object"
    (let [group (group/group [])]
      (is (= #{group}
             (zipper/get-all-matching-objects (#'zipper/new-group-zipper (group/group []))
                                              (constantly true))))))
  (testing "Returns nested objects"
    (let [group (group/group [shape-a])]
      (is (= #{group shape-a}
             (zipper/get-all-matching-objects (#'zipper/new-group-zipper (group/group []))
                                                       (constantly true)))))))



(deftest testing-update-objects
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
          zipper (zipper/create-zipper root)
          counter (ref 0)
          names (ref #{})]
      (let [updated-zipper (shared/update-objects zipper (fn [node]
                                                           (dosync
                                                            (let [new-name (str "name-" @counter)
                                                                  new-node (assoc node :name new-name)]                                                             
                                                              (alter counter inc)
                                                              (alter names #(conj % new-name))
                                                              new-node))))
            new-objects (zipper/get-all-matching-objects (#'zipper/new-group-zipper (shared/get-root updated-zipper))
                                                         (constantly true)) 
            set-names (into #{} (map :name new-objects))]
        (is (= @names
               set-names))))))
