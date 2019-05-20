(ns raytracer.shapes.plane-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.svector :as svector]
            [raytracer.shapes :as shapes]
            [raytracer.shapes.shared :as shared]
            [raytracer.point :as point]
            [raytracer.ray :as ray]            
            [raytracer.shapes.cube :as cube]
            [raytracer.shapes.shared :as shared]))

(def cube (cube/cube))

(defmacro test-intersection [id origin direction t1 t2]
  `(let [ray# (ray/ray (apply point/point ~origin)
                      (apply svector/svector ~direction))
         intersections# (shared/local-intersect cube ray#)]
     (testing ~(format "A ray intersects a cube (%s)" id)
       (is (v= [~t1 ~t2]
               (map :t intersections#))))))

(deftest test-cube-local-intersect
  (test-intersection "+x" [5, 0.5, 0] [-1, 0, 0] 4 6)
  (test-intersection "-x" [-5, 0.5, 0] [1, 0, 0] 4 6)
  (test-intersection "+y" [0.5, 5, 0] [0, -1, 0] 4 6)
  (test-intersection "-y" [0.5, -5, 0] [0, 1, 0] 4 6)
  (test-intersection "+z" [0.5, 0, 5] [0, 0, -1] 4 6)
  (test-intersection "-z" [0.5, 0, -5] [0, 0, 1] 4 6)
  (test-intersection "inside"  [0, 0.5, 0] [0, 0, 1] -1 1))

