(ns raytracer.world
  (:require [raytracer.tuple :as tuple]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.transform :as transform]
            [raytracer.ray :as ray]
            [raytracer.materials :as materials]
            [raytracer.light-sources :as light-sources]
            [raytracer.phong :as phong]))

(defn create []
  {:objects []
   :light-sources #{}})

(defn- add-object [world object]
  (update world :objects #(conj % object)))

(defn- add-light-source [world light-source]
  (update world :light-sources #(conj % light-source)))

(defn set-light-sources [world & light-sources]
  (assoc world :light-sources (vec light-sources)))

(defn default-world
  "A world used for testing.

  It kind of sucks conceptually that this function is here,
  but at least at the moment I'll keep it here because
  \"The Book made me do it.\""
  []
  (-> (create)
    (add-object (ray/change-material (ray/sphere)
                                     (materials/material :color [0.8 1.0 0.6]
                                                         :diffuse 0.7
                                                         :specular 0.2)))
    (add-object (ray/change-transform (ray/sphere)
                                      (transform/scale 0.5 0.5 0.5)))    
    (add-light-source (light-sources/create-point-light (point/point -10 10 -10)
                                                        [1 1 1]))))

(defn- unsorted-optimizations [world ray]
  (flatten
   (persistent!
    (reduce (fn [acc object]
              (conj! acc (ray/intersect ray object)))
            (transient [])
            (:objects world)))))

(defn intersect [world ray]
  (sort-by :t (unsorted-optimizations world ray)))

(defn- is-inside? [eye-v normal-v]
  (< (svector/dot eye-v normal-v) 0))


(defn prepare-computations [ray intersection]
  (let [point (tuple/add (:origin ray)
                         (svector/mul (:direction ray)
                                      (:t intersection)))
        eye-v (svector/neg (:direction ray))
        normal-v ((:normal (:object intersection)) point)
        inside (is-inside? eye-v normal-v)]
    {:inside inside
     :object (:object intersection)
     :t (:t intersection)
     :point  point
     :eye-v eye-v
     :normal-v (if inside (svector/neg normal-v) normal-v)}))

(defn shade-hit [world intermediate-result]
  (phong/lighting (-> intermediate-result :object :material)
                  (first (:light-sources world)) ;;; first light source, for now
                  (:point intermediate-result)
                  (:eye-v intermediate-result)
                  (:normal-v intermediate-result)))
