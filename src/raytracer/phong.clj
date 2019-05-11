(ns raytracer.phong
  (:require [raytracer.tuple :as tuple]
            [raytracer.svector :as svector]
            [raytracer.color :as color]
            [raytracer.materials :as materials]
            [raytracer.ray :as ray]))

(def EPSILON 1e-5)

(defn- compute-if-positive [value function]
  (if (< value 0)
    color/black
    (function value)))

(defn- compute-specular-factor [material reflect-dot-eye]
  (Math/pow reflect-dot-eye
            (:shiness material)))

(defn- compute-specular-value [reflect-dot-normal light-source material]
  (color/scale (:intensity light-source)
               (* (:specular material)
                  (compute-specular-factor material reflect-dot-normal))))

(defn- compute-specular [light-dot-normal neg-light-vector light-source normal eye material]
  (if (< light-dot-normal 0)
    color/black
    (compute-if-positive (tuple/dot (tuple/reflect neg-light-vector normal) eye)
                         #(compute-specular-value % light-source material))))

(defn- compute-diffuse-value [light-dot-normal effective-color material]
  (color/scale effective-color (* (:diffuse material)
                                  light-dot-normal)))

(defn- compute-diffuse [light-dot-normal effective-color material]
  (compute-if-positive light-dot-normal
                       #(compute-diffuse-value % effective-color material)))

(defn- compute-ambient [effective-color material]
  (color/scale effective-color (:ambient material)))

;;; TODO/FIXME hideous. Split in two methods lighting and shadow-lighting
(defn lighting
  ([object light-source position eye normal]
   (lighting object light-source position eye normal 1))
  ([object light-source position eye normal shadow-attenuation]
   (let [material (:material object)
         effective-color (color/mul (materials/get-color object position)
                                    (:intensity light-source))
         ambient-color (compute-ambient effective-color material)]
     (let [light-vector (tuple/normalize (tuple/sub (:position light-source) position))
           light-dot-normal (tuple/dot light-vector normal)]
       (if (< (Math/abs (float shadow-attenuation)) EPSILON)
         ambient-color
         (-> ambient-color
             (color/add
              (color/scale (compute-diffuse light-dot-normal effective-color material) shadow-attenuation))
             (color/add
              (color/scale (compute-specular light-dot-normal
                                               (tuple/mul light-vector -1)
                                               light-source
                                               normal eye material) shadow-attenuation))))))))
