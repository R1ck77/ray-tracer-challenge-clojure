(ns raytracer.shapes.group-integration-test
  (:require [clojure.test :refer :all]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.ray :as ray]
            [raytracer.shapes.group :as group]
            [raytracer.shapes.obj :as obj]
            [raytracer.shapes.shared :as shared]))

(defn- load-wavefront []
  (obj/obj (clojure.java.io/resource "obj/teapot.obj")))

(deftest test-local-intersect-on-large-group-bounding-box
  (testing "Intersection with a large group doesn't throw (bounding box)"
    (with-redefs [group/*use-bounding-boxes* true]
      (let [teapot (load-wavefront)
            ray (ray/ray (point/point 0 0.5 0)
                         (svector/svector 0 0 -1))]
        (shared/local-intersect teapot ray)))))

(deftest test-local-intersect-on-large-group-no-bounding-box
  (testing "Intersection with a large group doesn't throw (no bounding box)"
    (with-redefs [group/*use-bounding-boxes* false]
      (let [big-group (first (group/get-children (load-wavefront)))
           ray (ray/ray (point/point 0 0.5 0)
                        (svector/svector 0 0 -1))]
       (shared/local-intersect big-group ray)))))
