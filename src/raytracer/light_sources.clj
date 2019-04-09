(ns raytracer.light-sources)

(defn create-point-light [position intensity]
  {:position position
   :intensity intensity})
