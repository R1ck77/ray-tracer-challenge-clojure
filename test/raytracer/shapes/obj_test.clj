(ns raytracer.shapes.obj-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer [with-temp-file]]
            [raytracer.shapes.obj :as obj]))

(deftest test-obj
  (testing "obj returns a group with the non-empty objecs returned during parsing"
    (with-temp-file wavefront-file
      (spit wavefront-file "v -1 1 0 
v -1 0 0 
v 1 0 0 
v 1 1 0 
g FirstGroup
 
f 1 2 3 
g SecondGroup 
f 1 3 4")
      (let [result (obj/obj (.getAbsolutePath wavefront-file))]
        (is (= 2 (count (:children result))))))))
