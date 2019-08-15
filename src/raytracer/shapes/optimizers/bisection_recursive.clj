(ns raytracer.shapes.optimizers.bisection-recursive
  (:require [raytracer.shapes.optimizers.optimizer :as optimizer]
           [raytracer.shapes.group :as group]))

(defn- bisect-recursively [group max-size]
  (println "* Warning: optimization not yet implemented!")
  group)

(defn create [max-size]
  (reify optimizer/GroupOptimizer
    (optimize [this group]
      (bisect-recursively group max-size))))
