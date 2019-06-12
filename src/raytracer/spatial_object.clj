(ns raytracer.spatial-object)

(defprotocol SpatialObject
  (change-transform [this new-matrix])
  (inverse-transform [this point]))
