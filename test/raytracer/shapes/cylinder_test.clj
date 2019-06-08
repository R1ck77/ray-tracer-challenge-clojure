(ns raytracer.shapes.cylinder-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.ray :as ray]
            [raytracer.tuple :as tuple]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.transform :as transform]
            [raytracer.shapes.cylinder :as cylinder]
            [raytracer.shapes.shared :as shared]
            [raytracer.shapes :as shapes]))

(def a-cylinder (cylinder/cylinder))

(def √2 (Math/sqrt 2))
(def half√2 (/ √2 2))

(defmacro test-capped-intersections-count [name point direction n-intersections-expected]
  `(let [cylinder# (cylinder/cylinder :minimum 1,
                                      :maximum 2,
                                      :closed true)
         point# (apply point/point ~point)
         direction# (tuple/normalize (apply svector/svector ~direction))]
     (is (= ~n-intersections-expected
            (count (shared/local-intersect cylinder#
                                           (ray/ray point# direction#)))))))

(defmacro test-intersections-count [index point direction n-intersections-expected]
  `(let [cylinder# (cylinder/cylinder :minimum 1, :maximum 2)
         ray# (ray/ray (apply point/point ~point)
                       (tuple/normalize (apply svector/svector ~direction)))]
     (is (= ~n-intersections-expected (count (shared/local-intersect cylinder# ray#)))
         (format "capped cylinder intersection test #%d" ~index))))

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
    (assert-missed-intersection [0 0 0] [0 1 0])
    (assert-missed-intersection [0 0 -5] [1 1 1]))
  (testing "A ray hits the cylinder"
    (assert-intersection-hits 5 5 [1 0 -5] [0 0 1])
    (assert-intersection-hits 4 6 [0 0 -5] [0 0 1])
    (assert-intersection-hits 6.80798 7.08872 [0.5 0 -5] [0.1 1 1]))
  (testing "Intersecting a constrained cylinder"
    (test-intersections-count 1 [0 1.5 0] [0.1 1 0] 0)
    (test-intersections-count 2 [0 3 -5] [0 0 1] 0)
    (test-intersections-count 3 [0 0 -5] [0 0 1] 0)
    (test-intersections-count 4 [0 2 -5] [0 0 1] 0)
    (test-intersections-count 5 [0 1 -5] [0 0 1] 0)
    (test-intersections-count 6 [0 1.5 -2] [0 0 1] 2))
  (testing "Intersecting the caps of a closed cylinder"
    (test-capped-intersections-count "1" [0 3 0] [0 -1 0] 2)
    (test-capped-intersections-count "2" [0 3 -2] [0 -1 2] 2)
    (test-capped-intersections-count "3 - corner case" [0 4 -2] [0 -1 1] 2)
    (test-capped-intersections-count "4" [0 0 -2] [0 1 2] 2)
    (test-capped-intersections-count "5 - corner case" [0 -1 -2] [0 1 1] 2)))

(deftest test-compute-normal
  (testing "Normal vector on a cylinder"
    (is (t= (svector/svector 1 0 0)
            (shared/compute-normal a-cylinder (point/point 1 0 0))))
    (is (t= (svector/svector 0 0 -1)
            (shared/compute-normal a-cylinder (point/point 0 5 -1))))
    (is (t= (svector/svector 0 0 1)
            (shared/compute-normal a-cylinder (point/point 0 -2 1))))
    (is (t= (svector/svector -1 0 0)
            (shared/compute-normal a-cylinder (point/point -1 1 0)))))
  (testing "Extra normal tests not in the book"
    (is (t= (svector/svector 0 0 -1)
            (shared/compute-normal a-cylinder (point/point 0 5 -1.1))))
    (is (t= (svector/svector half√2 0 (- half√2))
            (shared/compute-normal a-cylinder (point/point half√2 100 (- half√2))))))
  (testing "The normal vector on a cylinder's end caps"
    (let [cylinder (cylinder/cylinder :minimum 1
                                      :maximum 2
                                      :closed true)]
      (is (t= (svector/svector 0 -1 0) (shared/compute-normal cylinder (point/point 0 1 0))))
      (is (t= (svector/svector 0 -1 0) (shared/compute-normal cylinder (point/point 0.5 1 0))))
      (is (t= (svector/svector 0 -1 0) (shared/compute-normal cylinder (point/point 0 1 0.5))))
      (is (t= (svector/svector 0 1 0) (shared/compute-normal cylinder (point/point 0 2 0))))
      (is (t= (svector/svector 0 1 0) (shared/compute-normal cylinder (point/point 0.5 2 0))))
      (is (t= (svector/svector 0 1 0) (shared/compute-normal cylinder (point/point 0 2 0.5)))))))


(deftest test-equality
  (testing "Two cylinders are equals if the share the same characteristics"
    (is (= (cylinder/cylinder)
           (cylinder/cylinder)))
    (let [transformed-cylinder (shapes/change-transform a-cylinder
                                                        (transform/translate 1 2 3 ))]
      (is (= transformed-cylinder transformed-cylinder))
      (is (not= transformed-cylinder (cylinder/cylinder))))))

(deftest test-constructor
  (testing "The default minimum and maximum for a cylinder"
    (is (= Double/POSITIVE_INFINITY (:maximum a-cylinder)))
    (is (= Double/NEGATIVE_INFINITY (:minimum a-cylinder))))
  (testing "A cylinder can be created with a set minimum"
    (let [cylinder (cylinder/cylinder :minimum 12)]
      (is (= Double/POSITIVE_INFINITY (:maximum cylinder)))
      (is (= 12 (:minimum cylinder)))))
  (testing "A cylinder can be created with a set maximum"
    (let [cylinder (cylinder/cylinder :maximum 100)]
      (is (= 100 (:maximum cylinder)))
      (is (= Double/NEGATIVE_INFINITY (:minimum a-cylinder)))))
  (testing "A cylinder can be created with a set minimum and maximum"
    (let [cylinder (cylinder/cylinder :maximum 100 :minimum 12)]
      (is (= 12 (:minimum cylinder)))
      (is (= 100 (:maximum cylinder)))))
  (testing "The default closed value for a cylinder"
    (is (not (:closed a-cylinder))))
  (testing "Filled cylinders can be created"
    (is (:closed (cylinder/cylinder :closed true)))))


