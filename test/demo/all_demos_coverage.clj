(ns demo.all-demos-coverage
  (:require [clojure.test :refer :all]
            [raytracer.world :as world]
            [demo.projectile :as projectile]
            [demo.clock :as clock]
            [demo.simple-projection :as simple-projection]
            [demo.phong-sphere :as phong-sphere]
            [demo.phong-scene :as phong-scene]
            [demo.plane-demo :as plane-demo]
            [demo.fresnel-demo :as fresnel]
            [demo.perlin-demo :as perlin]
            [demo.pattern-demo :as pattern-demo]
            [demo.reflection-demo :as reflections-demo]
            [demo.cubes-demo :as cubes-demo]
            [demo.cone-demo :as cone-demo]))


(defmacro run-demo [name demo-code]
  `(do
     (println ~name)
     ~demo-code))

(defn run-all-demos []
  (run-demo "projectile demo"
    (projectile/simulate-dragless-cannon 45 23))
  (run-demo "clock demo"
    (clock/render-demo))
  (run-demo "projection demo"
    (simple-projection/render-demo))
  (run-demo "tiny phong sphere demo"
    (phong-sphere/render-demo 8 4))
  (run-demo "tiny phong scene demo"
    (phong-scene/render-demo 10 5))
  (run-demo "tiny plane demo"
            (plane-demo/render-demo 10 5))
  (run-demo "tiny pattern demo"
            (pattern-demo/render-demo 10 5))
  (run-demo "tiny reflections demo"
            (with-redefs [world/*maximum-reflections* 2]
              (reflections-demo/render-demo 10 5)))
  (run-demo "tiny cubes demo"
            (with-redefs [world/*maximum-reflections* 2]
              (cubes-demo/render-demo 10 5)))
  (run-demo "tiny cylinders and cones demo"
            (with-redefs [world/*maximum-reflections* 2]
              (cone-demo/render-demo 10 5)))  
  (run-demo "tiny fresnel demo"
            (fresnel/render-demo 10 5))
  (run-demo "tiny perlin demo"
            (do
              (perlin/render-demo 10 10)
              (perlin/render-animation 10 10 3))))

(defn -main [& args]
  (run-all-demos)
  (shutdown-agents))
