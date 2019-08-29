(ns raytracer.material
  (:require [raytracer.point :as point]
            [raytracer.color :as color]
            [raytracer.pattern :as pattern]
            [raytracer.shapes.shared :as shared]))

(defrecord Material
    [ambient diffuse specular shiness
     reflectivity refractive-index
     transparency])

(defn material
  ([]
   (map->Material {:ambient 0.1
                   :diffuse 0.9
                   :specular 0.9
                   :shiness 200.0
                   :reflectivity 0.0
                   :transparency 0.0
                   :refractive-index 1.0
                   :pattern (pattern/solid (color/color 1 1 1))}))
  ([& {:as args}]
   (merge (material) args)))

(defn with-color [color & {:as args}]
  (apply material (mapcat seq (merge args {:pattern (pattern/solid color)}))))

(def void-material (with-color (color/color 0 0 0)
                               :transparency 1.0
                               :refractive-index 1.0))

(def some-point (point/point 0 0 0))
(def some-object ())

(defn get-color
  ([material]
   (pattern/color-at (-> material :pattern) some-point))
  ([object point]
   (pattern/color-at (-> object shared/get-material :pattern) object point)))

(defn update-material [material & {:as args}]
  (merge material args))

(def default-material (material))
