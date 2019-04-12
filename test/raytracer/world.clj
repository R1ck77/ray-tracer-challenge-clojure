(ns raytracer.world
  (:require [raytracer.point :as point]
            [raytracer.transform :as transform]
            [raytracer.ray :as ray]
            [raytracer.materials :as materials]
            [raytracer.light-sources :as light-sources]))

(defn create []
  {:objects #{}
   :light-sources #{}})

(defn- add-object [world object]
  (update world :objects #(conj % object)))

(defn- add-light-source [world light-source]
  (update world :light-sources #(conj % light-source)))

(defn default-world
  "A world used for testing.

  It kind of sucks conceptually that this function is here,
  but at least at the moment I'll keep it here because
  \"The Book made me do it.\""
  []
  (-> (create)
    (add-object (ray/change-transform (ray/sphere)
                                      (transform/scale 0.5 0.5 0.5)))
    (add-object (ray/change-material (ray/sphere)
                                     (materials/material :color [0.8 1.0 0.6]
                                                         :diffuse 0.7
                                                         :specular 0.2)))
    (add-light-source (light-sources/create-point-light (point/point -10 10 -10)
                                                        [1 1 1]))))

(defn- unsorted-optimizations [world ray]
  (flatten
   (persistent!
    (reduce (fn [acc object]
              (conj! acc (:values (ray/intersect ray object))))
            (transient [])
            (:objects world)))))

(defn intersect [world ray]
  (sort-by :t (unsorted-optimizations world ray)))

