(ns demo.all-demos-coverage
  (:require [clojure.test :refer :all]
            [demo.projectile :as projectile]
            [demo.clock :as clock]
            [demo.simple-projection :as simple-projection]
            [demo.phong-sphere :as phong-sphere]
            [demo.phong-scene :as phong-scene]
            [demo.plane-demo :as plane-demo]))


(defmacro run-demo [name demo-code]
  `(do
     (println ~name)
     ~demo-code))

(defn run-all-demos []
  (run-demo "projectile demo"
    (projectile/simulate-dragless-cannon 45 23))
  (run-demo "clock demo"
    (clock/render-demo))
  (run-demo "simple projection demo"
    (simple-projection/render-demo))
  (run-demo "tiny phong sphere demo"
    (phong-sphere/render-demo 8 4))
  (run-demo "tiny phong scene demo"
    (phong-scene/render-demo 10 5))
  (run-demo "tiny plane demo"
    (plane-demo/render-demo 10 5)))

(defn -main [& args]
  (run-all-demos))
