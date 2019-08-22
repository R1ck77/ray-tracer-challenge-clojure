(ns raytracer.wavefront.parser-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :as tu]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.shapes.triangle :as triangle]
            [raytracer.shapes.smooth-triangle :as smooth-triangle]
            [raytracer.wavefront.parser :as parser]))

(deftest test-parse-vertex-data
  (testing "Ignoring unrecognized lines"
    (let [result (parser/parse "There was a young lady named Bright
who traveled much faster than light.
She set out one day
in a relative way,
and came back the previous night.")]
      (is (= 5 (count (:ignored result))))))
  (testing "Vertex records"
    (let [result (parser/parse "v -1 1 0
v -1.0000 0.5000 0.0000
v 1 0 0
v 1 1 0")]
      (is (tu/t= (point/point -1 1 0)
                 (get (:vertices result) 1)))
      (is (tu/t= (point/point -1 0.5 0)
                 (get (:vertices result) 2)))
      (is (tu/t= (point/point 1 0 0)
                 (get (:vertices result) 3)))
      (is (tu/t= (point/point 1 1 0)
                 (get (:vertices result) 4)))))
  (testing "Vertex normal records"
    (let [result (parser/parse "vn 0 0 1
vn 0.707 0 -0.707
vn 1 2 3")]
      (is (tu/t= (svector/svector 0 0 1)
                 (get (:normals result) 1)))
      (is (tu/t= (svector/svector 0.707 0 -0.707)
                 (get (:normals result) 2)))
      (is (tu/t= (svector/svector 1 2 3)
                 (get (:normals result) 3))))))

(deftest test-parse-faces
  (testing "Parsing triangle faces"
    (let [result (parser/parse "v -1 1 0
v -1 0 0
v 1 0 0
v 1 1 0
f 1 2 3
f 1 3 4")
          vertices (:vertices result)
          default-group (-> result :groups :default-group)]
      (is (= (triangle/triangle (get vertices 1)
                                (get vertices 2)
                                (get vertices 3))
             (get default-group 1)))
      (is (= (triangle/triangle (get vertices 1)
                                (get vertices 3)
                                (get vertices 4))
             (get default-group 2)))))  
  (testing "Parsing polygon faces"
    (let [results (parser/parse "v -1 1 0
v -1 0 0
v 1 0 0
v 1 1 0
v 0 2 0
f 1 2 3 4 5")
          vertices (:vertices results)
          default-group (-> results :groups :default-group)]
      (is (= (triangle/triangle (get vertices 1)
                                (get vertices 2)
                                (get vertices 3))
             (get default-group 1)))
      (is (= (triangle/triangle (get vertices 1)
                                (get vertices 3)
                                (get vertices 4))
             (get default-group 2)))
      (is (= (triangle/triangle (get vertices 1)
                                (get vertices 4)
                                (get vertices 5))
             (get default-group 3)))))
  (testing "Faces with normals"
    (let [result (parser/parse "v 0 1 0
v -1 0 0
v 1 0 0

vn -1 0 0
vn 1 0 0
vn 0 1 0

f 1//3 2//1 3//2
f 1/0/3 2/102/1 3/14/2 ")
          default-group (-> result :groups :default-group)
          [_ t1 t2] default-group]
      (is (= (smooth-triangle/smooth-triangle (point/point 0.0 1.0 0.0)
                                              (point/point -1.0 0.0 0.0)
                                              (point/point 1.0 0.0 0.0)
                                              (svector/svector 0.0 1.0 0.0)
                                              (svector/svector -1.0 0.0 0.0)
                                              (svector/svector 1.0 0.0 0.0))
             t1))
      (is (= t1 t2)))))

(deftest test-parse-groups
  (testing "Triangles in groups"
    (let [results (parser/parse "v -1 1 0 
v -1 0 0 
v 1 0 0 
v 1 1 0 
g FirstGroup
 
f 1 2 3 
g SecondGroup 
f 1 3 4")
          vertices (:vertices results)
          groups (:groups results)
          default-group (:default-group groups)
          first-group (get groups "FirstGroup")
          second-group (get groups "SecondGroup")]
      (is (= [nil] default-group))
      (is (= [nil
              (triangle/triangle (get vertices 1)
                                 (get vertices 2)
                                 (get vertices 3))]
             first-group))
      (is (= [nil
              (triangle/triangle (get vertices 1)
                                 (get vertices 3)
                                 (get vertices 4))]
             second-group)))))
