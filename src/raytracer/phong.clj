(ns raytracer.phong
  (:require [raytracer.tuple :as tuple]
            [raytracer.svector :as svector]
            [raytracer.color :as color]
            [raytracer.ray :as ray]))

(defn- compute-specular-factor [material reflect-dot-eye]
  (Math/pow reflect-dot-eye
            (:shiness material)))

(defn- compute-specular [light-dot-normal neg-light light-source normal eye material]
  (if (< light-dot-normal 0)
    color/black
    (let [reflect-dot-eye (svector/dot (ray/reflect neg-light normal)
                                       eye)]
      (if (< reflect-dot-eye 0)
        color/black
        (tuple/mul (:intensity light-source)
                   (* (:specular material)
                      (compute-specular-factor material reflect-dot-eye)))))))

(defn- compute-diffuse [light-dot-normal effective-color material]
  (if (< light-dot-normal 0)
    color/black
    (tuple/mul effective-color (* (:diffuse material)
                                  light-dot-normal))))

(defn- compute-ambient [effective-color material]
  (tuple/mul effective-color (:ambient material)))

;;; TODO/FIXME compute color material ambient
(defn lighting [material light-source position eye normal]
  (let [effective-color (color/mul (:color material)
                                   (:intensity light-source))
        light (svector/normalize (tuple/sub (:position light-source) position))
        light-dot-normal (svector/dot light normal)]
    (tuple/add (tuple/add (compute-ambient effective-color material)
                (compute-diffuse light-dot-normal effective-color material))
                (compute-specular light-dot-normal
                                  (svector/mul light -1)
                                  light-source
                                  normal eye material))))
