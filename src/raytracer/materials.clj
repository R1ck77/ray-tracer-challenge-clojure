(ns raytracer.materials
  (:require [raytracer.pattern :as pattern])) ;;;TODO/FIXME change the name to material

(defn material
  ([]
   {:color [1 1 1]
    :ambient 0.1
    :diffuse 0.9
    :specular 0.9
    :shiness 200.0
    :pattern nil}) ;; TODO/FIXME very crude. generalize in a color getter function and remove color!
                   ;; you can probably fit a texture, uv mapping or something here, and make color a special case!
  ([& {:as args}]
   (merge (material) args)))


(defn get-color [material point]
  (if-let [pattern (:pattern material)]
    (pattern/color-at-object pattern nil point)
    (:color material)))

