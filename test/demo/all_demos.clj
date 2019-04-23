(ns demo.all-demos
  (:require [clojure.test :refer :all]
            [demo.projectile :as projectile]
            [demo.clock :as clock]
            [demo.simple-projection :as simple-projection]
            [demo.phong-sphere :as phong-sphere]
            [demo.phong-scene :as phong-scene]
            [demo.plane-demo :as plane-demo]))

(deftest test-demo
  (testing "projectile demo"
    (projectile/simulate-dragless-cannon 45 23))
  (testing "clock demo"
    (clock/render-demo))
  (testing "simple projection demo"
    (simple-projection/render-demo))
  (testing "tiny phong sphere demo"
    (phong-sphere/render-demo 32 20))
  (testing "tiny phong scene demo"
    (phong-scene/render-demo 40 20))
  (testing "tiny plane demo"
    (plane-demo/render-demo 40 20)))
