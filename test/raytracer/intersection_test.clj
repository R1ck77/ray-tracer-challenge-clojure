(ns raytracer.intersection-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]            
            [raytracer.intersection :as intersection]
            [raytracer.ray :as ray]
            [raytracer.transform :as transform]
            [raytracer.materials :as materials]))

(deftest test-intersection
  (testing "create new intersection"
    (let [sphere (ray/sphere)
          intersection (intersection/intersection 2.4 sphere)]
      (is (identical? sphere (:object intersection)))
      (is (= 2.4 (:t intersection))))))

(deftest test-intersections
  (testing "exciting \"intersections\" function"
    (let [i1 (intersection/intersection 2.4 (ray/sphere))
          i2 (intersection/intersection 2.8 (ray/sphere))
          intersections (intersection/intersections i1 i2)]
      (is (= 2 (count intersections)))
      (is (identical? i1 (first intersections)))
      (is (identical? i2 (second intersections))))))

(defmacro hit-testcase [ & {:keys [message intersections-t expected]}]
  `(testing ~message
     (let [sphere# (ray/sphere)
           intersections# (apply intersection/intersections (map #(intersection/intersection % sphere#) ~intersections-t))
           hit# (intersection/hit intersections#)]
       (is (or (and (not ~expected) (not hit#))
               (= (:t (nth intersections# ~expected)) (:t hit#)))))))
(deftest test-hit
  (hit-testcase :message "the hit, when all intersections have positive t"
                :intersections-t [1 2]
                :expected 0)
  (hit-testcase :message "the hit, when some intersections have negative t"
                :intersections-t [-1 1]
                :expected 1)
  (hit-testcase :message "the hit, when all intersections have negative t"
                :intersections-t [-2 -1]
                :expected nil)
  (hit-testcase :message "the hit is always the lowest nonnegative intersection"
                :intersections-t [5 7 -3 2 4]
                :expected 3))
