(ns raytracer.materials) ;;;TODO/FIXME change the name to material

(defn material
  ([]
   {:color [1 1 1]
    :ambient 0.1
    :diffuse 0.9
    :specular 0.9
    :shiness 200.0})
  ([& {:as args}]
   (merge (material) args)))


