(ns raytracer.materials ;;;TODO/FIXME change the name to material
  (:require [raytracer.color :as color]
            [raytracer.pattern :as pattern]))

(defrecord Material
    [color
     ambient diffuse specular shiness
     reflectivity refractive-index
     transparency])

(defn material
  ([]
   (map->Material {:color (color/color 1 1 1)
                   :ambient 0.1
                   :diffuse 0.9
                   :specular 0.9
                   :shiness 200.0
                   :reflectivity 0.0
                   :transparency 0.0
                   :refractive-index 1.0
                   :pattern nil})) ;; TODO/FIXME very crude. generalize in a color getter function and remove color!
                                   ;; you can probably fit a texture, uv mapping or something here, and make color a special case!
  ([& {:as args}]
   (merge (material) args)))

(def void-material (material :color (color/color 0 0 0)
                             :transparency 1.0
                             :refractive-index 1.0))

(defn get-color [object point]
  (let [material (:material object)]
    (if-let [pattern (:pattern material)]
      (pattern/color-at-object pattern object point)
      (:color material))))

(defn update-material [material & {:as args}]
  (merge material args))

