(ns raytracer.material
  (:require [raytracer.color :as color]
            [raytracer.pattern :as pattern]))

(defrecord Material
    [color
     ambient diffuse specular shiness
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

(def void-material (material :transparency 1.0
                             :refractive-index 1.0
                             :pattern (pattern/solid (color/color 0 0 0))))

(defn get-color [object point]
  (pattern/color-at (-> object :material :pattern) object point))

(defn update-material [material & {:as args}]
  (merge material args))

