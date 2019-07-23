(ns raytracer.bounding-box)

(defprotocol BoundingBox
  (get-sides [this] "Return each side of the box")
  (get-vertices [this] "Return the set of all vertices"))

(defn transform
  "Return an axis aligned bounding box transformed by transform"
  [this transform] )

(defn merge
  "Return a bounding box that contains both boxes"
  [this other] )
