(ns raytracer.shapes.cube
  (:require [raytracer.tuple :as tuple]
            [raytracer.svector :as svector]
            [raytracer.color :as color]
            [raytracer.material :as material]
            [raytracer.shapes.shared :as shared]))

(defrecord Cube [material transform inverse-transform])

(defn- local-intersect [cube ray])

(extend-type Cube
  shared/Intersectable
  (local-intersect [this ray-object-space]
   (local-intersect this ray-object-space)))

(defn cube []
  (->Cube nil nil nil))

