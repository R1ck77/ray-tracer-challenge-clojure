(ns raytracer.shapes.bounding-box)

(defprotocol BoundingBox
  (get-sides [this] "Return each side of the box")
  (get-extremes [this] "Return the two extremes vertices of the box"))

(def unit-cube (reify BoundingBox
                 (get-sides [this]
                   [2 2 2])
                 (get-extremes [this]
                   #{[-1 -1 -1] [1 1 1]})))

(defn- filter-points [points f]
  {:pre [(not (empty? points))]}
  (reduce (fn [accumulator point]
            (map f accumulator point))
          points))

(defn extremes-from-points [points]
  {:pre [(not (empty? points))]}  
  (vector (filter-points points min)
          (filter-points points max)))

(defn transform
  "Return an axis aligned bounding box transformed by transform"
  [this transform]
  ;;; Transform all the points and then get the new bounding box
  )

(defn merge
  "Return a bounding box that contains both boxes"
  [this other]
  ;;; Create the union of the points and get the new bounding box
  )
