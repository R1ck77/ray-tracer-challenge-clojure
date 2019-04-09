(ns raytracer.light-sources-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.light-sources :as light-sources]
            [raytracer.point :as point]
            [raytracer.svector :as svector]))

(deftest test-create-point-light
  (testing "a point light has a position and intensity"
    (let [light (light-sources/create-point-light (point/point 0 0 0)
                                          [1 1 1])]
      (is (= (point/point 0 0 0)
             (:position light)))
      (is (= [1 1 1]
             (:intensity light))))))
