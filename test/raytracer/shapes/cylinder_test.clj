(ns raytracer.shapes.cylinder-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.ray :as ray]
            [raytracer.tuple :as tuple]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.shapes.cylinder :as cylinder]
            [raytracer.shapes.shared :as shared]))

(def a-cylinder (cylinder/cylinder))

(defn assert-intersection-hits [t1 t2 origin direction]
  (is (v= [t1 t2]
          (map :t (shared/local-intersect a-cylinder
                                          (ray/ray (apply point/point origin)
                                                   (tuple/normalize (apply svector/svector direction))))))))

(defn assert-missed-intersection [origin direction]
  (is (empty? (shared/local-intersect a-cylinder
                                      (ray/ray (apply point/point origin)
                                               (tuple/normalize (apply svector/svector direction)))))))

(deftest test-local-intersect
  (testing "A ray misses a cylinder"
    (assert-missed-intersection [1 0 0] [0 1 0])
    (assert-missed-intersection [0 0, 0] [0 1 0])
    (assert-missed-intersection [0 0 -5] [1 1 1]))
  (testing "A ray hits the cylinder"
    (assert-intersection-hits 5 5 [1 0 -5] [0 0 1])
    (assert-intersection-hits 4 6 [0 0 -5] [0 0 1])
    (assert-intersection-hits 6.80798 7.08872 [0.5 0 -5] [0.1 1 1])))
