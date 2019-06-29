(ns raytracer.grouping.shared)

(defprotocol CoordinatesConverter
  (local-to-world-coordinates [this shape svector])
  (world-to-local-coordinates [this shape point]))
