(ns raytracer.phong
  (:require [raytracer.tuple :as tuple]
            [raytracer.svector :as svector]
            [raytracer.color :as color]
            [raytracer.ray :as ray]))

(defn- compute-if-positive [value function]
  (if (< value 0)
    color/black
    (function value)))

(defn- compute-specular-factor [material reflect-dot-eye]
  (Math/pow reflect-dot-eye
            (:shiness material)))

(defn- compute-specular-value [reflect-dot-normal light-source material]
  (tuple/mul (:intensity light-source)
             (* (:specular material)
                (compute-specular-factor material reflect-dot-normal))))

(defn- compute-specular [light-dot-normal neg-light-vector light-source normal eye material]
  (if (< light-dot-normal 0)
    color/black
    (compute-if-positive (svector/dot (ray/reflect neg-light-vector normal) eye)
                         #(compute-specular-value % light-source material))))

(defn- compute-diffuse-value [light-dot-normal effective-color material]
  (tuple/mul effective-color (* (:diffuse material)
                                light-dot-normal)))

(defn- compute-diffuse [light-dot-normal effective-color material]
  (compute-if-positive light-dot-normal
                       #(compute-diffuse-value % effective-color material)))

(defn- compute-ambient [effective-color material]
  (tuple/mul effective-color (:ambient material)))

;;; TODO/FIXME compute color material ambient
(defn lighting [material light-source position eye normal]
  (let [effective-color (color/mul (:color material)
                                   (:intensity light-source))
        light-vector (svector/normalize (tuple/sub (:position light-source) position))
        light-dot-normal (svector/dot light-vector normal)]
    (tuple/add (compute-ambient effective-color material)
               (compute-diffuse light-dot-normal effective-color material)
               (compute-specular light-dot-normal
                                  (svector/mul light-vector -1)
                                  light-source
                                  normal eye material))))
