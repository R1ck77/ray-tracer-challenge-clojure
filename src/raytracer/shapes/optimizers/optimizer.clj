(ns raytracer.shapes.optimizers.optimizer)

(defprotocol GroupOptimizer
  (optimize [this group]))
