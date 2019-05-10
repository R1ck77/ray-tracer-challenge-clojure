(ns raytracer.light-sources)

(defrecord LightSource [position intensity])

(defn create-point-light [position intensity]
  (->LightSource position intensity))
