(ns raytracer.shapes.cube-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.svector :as svector]
            [raytracer.shapes :as shapes]
            [raytracer.shapes.shared :as shared]
            [raytracer.point :as point]
            [raytracer.matrix :as matrix]
            [raytracer.transform :as transform]
            [raytracer.ray :as ray]            
            [raytracer.shapes.cube :as cube]
            [raytracer.shapes.shared :as shared]
            [raytracer.shapes.bounding-box :as bounding-box]
            [raytracer.shapes.placement :as placement]
            [raytracer.grouping.hierarchy :as hierarchy]))

(def a-cube (cube/cube))

(deftest test-constructor
  (testing "The courtesy construction function creates a cube with material, inverse transforms and inverse transposed transform set"
    (let [cube (cube/cube)]
      (is (:material cube))
      (is (v= matrix/identity-matrix (-> cube :placement placement/get-inverse-transform)))
      (is (v= matrix/identity-matrix (-> cube :placement placement/get-inverse-transposed-transform))))))

(defmacro test-no-intersection [name origin direction]
 `(testing ~(format "A ray misses a cube (%s)" name)
   (let [ray# (ray/ray (apply point/point ~origin) (apply svector/svector ~direction))]
     (is (empty? (shared/local-intersect a-cube ray#))))))

(defmacro test-intersection
  "Generate a test for the intersection between the AABB and a ray"
  [id origin direction t1 t2]
  `(let [ray# (ray/ray (apply point/point ~origin)
                      (apply svector/svector ~direction))
         intersections# (shared/local-intersect a-cube ray#)]
     (testing ~(format "A ray intersects a cube (%s)" id)
       (is (v= [~t1 ~t2]
               (map :t intersections#))))))

(deftest test-cube-local-intersect
  (test-intersection "+x" [5 0.5 0] [-1 0 0] 4 6)
  (test-intersection "-x" [-5 0.5 0] [1 0 0] 4 6)
  (test-intersection "+y" [0.5 5 0] [0 -1 0] 4 6)
  (test-intersection "-y" [0.5 -5 0] [0 1 0] 4 6)
  (test-intersection "+z" [0.5 0 5] [0 0 -1] 4 6)
  (test-intersection "-z" [0.5 0 -5] [0 0 1] 4 6)
  (test-intersection "inside"  [0 0.5 0] [0 0 1] -1 1)
  (test-no-intersection "case 0" [-2 0 0] [0.2673 0.5345 0.8018])
  (test-no-intersection "case 1" [0 -2 0] [0.8018 0.2673 0.5345])
  (test-no-intersection "case 2" [0 0 -2] [0.5345 0.8018 0.2673])
  (test-no-intersection "case 3" [2 0 2] [0 0 -1])
  (test-no-intersection "case 4" [0 2 2] [0 -1 0])
  (test-no-intersection "case 5" [2 2 0] [-1 0 0]))

(defmacro test-compute-normal [id point expected-normal]
  `(testing ~(format "The normal on the surface of a cube (%s)" id)
     (is (t= (apply svector/svector ~expected-normal)
             (shared/compute-normal a-cube
                                    (apply point/point ~point)
                                    (hierarchy/hierarchy a-cube))))))

(deftest test-cube-compute-normal
  (test-compute-normal "+x" [1 0.5 -0.8] [1 0 0])
  (test-compute-normal "-x" [-1 -0.2 0.9] [-1 0 0])
  (test-compute-normal "+y" [-0.4 1 -0.1] [0 1 0])
  (test-compute-normal "-y" [0.3 -1 -0.7] [0 -1 0])
  (test-compute-normal "+z" [-0.6 0.3 1] [0 0 1])
  (test-compute-normal "-z" [0.4 0.4 -1] [0 0 -1])
  (test-compute-normal "upper corner" [1 1 1] [1 0 0])
  (test-compute-normal "lower corner" [-1 -1 -1] [-1 0 0])
  (testing "normal is computed accounting for the cube transforms"
    (let [transformed-cube (shared/change-transform a-cube
                                             (transform/rotate-y Math/PI))]
      (is (t= (svector/svector 0 0 -1)
              (shared/compute-normal transformed-cube
                                     (point/point 0.2 0.1 -0.5)
                                     (hierarchy/hierarchy transformed-cube))))))) 

(deftest test-includes?
  (let [shape (cube/cube)]
    (testing "The shape includes itself"
      (is (shared/includes? shape shape)))
    (testing "The shape does not include a copy of itself"
      (is (not (identical? (cube/cube) (cube/cube))))
      (is (not (shared/includes? shape (cube/cube)))))
    (testing "The shape does not include a different object"
      (is (not (shared/includes? shape (shapes/plane)))))))
