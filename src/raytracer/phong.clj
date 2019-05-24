(ns raytracer.phong
  (:require [raytracer.const :as const]
            [raytracer.tuple :as tuple]
            [raytracer.svector :as svector]
            [raytracer.color :as color]
            [raytracer.material :as material]
            [raytracer.ray :as ray]))

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


(defn- scaled-color [color shadow-attenuation]
  (color/scale color shadow-attenuation))

(defn- full-phong-lighting [ambient-color material effective-color light-source light-vector eye normal shadow-attenuation]
  (let [light-dot-normal (tuple/dot light-vector normal)
        diffuse (compute-diffuse light-dot-normal effective-color material)
        specular (compute-specular light-dot-normal
                                   (tuple/mul light-vector -1)
                                   light-source
                                   normal eye material)]
    (-> ambient-color
        (color/add (scaled-color diffuse shadow-attenuation))
        (color/add (scaled-color specular shadow-attenuation)))))

(defn- compute-ambient [effective-color material]
  (color/scale effective-color (:ambient material)))

(defn lighting
  ([object light-source position eye normal]
   (lighting object light-source position eye normal 1))
  ([object light-source position eye normal shadow-attenuation]
   (if (nil? light-source)
     color/black
     (let [effective-color (color/mul (material/get-color object position)
                                      (:intensity light-source))
           ambient-color (compute-ambient effective-color (:material object))]
       (if (< (Math/abs (float shadow-attenuation)) const/EPSILON)
         ambient-color
         (let [light-vector (tuple/normalize (tuple/sub (:position light-source) position))]
           (full-phong-lighting ambient-color (:material object) effective-color light-source light-vector eye normal shadow-attenuation)))))))
