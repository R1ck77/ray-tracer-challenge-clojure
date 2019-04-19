(ns raytracer.world
  (:require [raytracer.tuple :as tuple]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.transform :as transform]
            [raytracer.ray :as ray]
            [raytracer.shapes :as shapes]
            [raytracer.intersection :as intersection]
            [raytracer.matrix :as matrix]
            [raytracer.materials :as materials]
            [raytracer.light-sources :as light-sources]
            [raytracer.phong :as phong]))

(def EPSILON 1e-6)

(defn create []
  {:objects []
   :light-sources #{}})

(defn- add-object [world object]
  (update world :objects #(conj % object)))

(defn- add-light-source [world light-source]
  (update world :light-sources #(conj % light-source)))

(defn set-light-sources [world & light-sources]
  (assoc world :light-sources (vec light-sources)))

(defn set-objects [world objects]
  (assoc world :objects (vec objects)))

(defn default-world
  "A world used for testing.

  It kind of sucks conceptually that this function is here,
  but at least at the moment I'll keep it here because
  \"The Book made me do it.\""
  []
  (-> (create)
    (add-object (shapes/change-material (shapes/sphere)
                                        (materials/material :color [0.8 1.0 0.6]
                                                         :diffuse 0.7
                                                         :specular 0.2)))
    (add-object (shapes/change-transform (shapes/sphere)
                                      (transform/scale 0.5 0.5 0.5)))    
    (add-light-source (light-sources/create-point-light (point/point -10 10 -10)
                                                        [1 1 1]))))

(defn- unsorted-intersections [world ray]
  (flatten
   (persistent!
    (reduce (fn [acc object]
              (conj! acc (ray/intersect ray object)))
            (transient [])
            (:objects world)))))

(defn intersect [world ray]
  (sort-by :t (unsorted-intersections world ray)))

(defn- is-inside? [eye-v normal-v]
  (< (svector/dot eye-v normal-v) 0))


(defn prepare-computations [ray intersection]
  (let [point (tuple/add (:origin ray)
                         (svector/mul (:direction ray)
                                      (:t intersection)))
        eye-v (svector/neg (:direction ray))
        normal-v ((:normal (:object intersection)) point)
        inside (is-inside? eye-v normal-v)
        normal-v (if inside (svector/neg normal-v) normal-v)]
    {:inside inside
     :object (:object intersection)
     :t (:t intersection)
     :point  point
     :over-point (tuple/add point (svector/mul normal-v EPSILON))
     :eye-v eye-v
     :normal-v normal-v}))

(defn is-shadowed? [world point]
  (let [light-source (first (:light-sources world)) ;;; first light source only
        pos->light (tuple/sub (:position light-source) point)
        intersection (intersection/hit (intersect world (ray/ray point (svector/normalize pos->light))))]
    (and intersection
         (< (:t intersection) (svector/mag pos->light)))))

;; TODO/FIXME the rendering throws without a light source set!
(defn shade-hit [world intermediate-result]
  (let [shadowed (is-shadowed? world (:over-point intermediate-result))]
    (phong/lighting (-> intermediate-result :object :material)
                    (first (:light-sources world)) ;;; first light source, for now
                    (:point intermediate-result)
                    (:eye-v intermediate-result)
                    (:normal-v intermediate-result)
                    shadowed)))

(defn color-at [world ray]
  (let [intersections (intersect world ray)]
    (if (empty? intersections)
      [0 0 0]
      (shade-hit world (prepare-computations ray (intersection/hit intersections))))))

(defn view-transform [[from-x from-y from-z _ :as from] to up]
  (let [[fwd-x fwd-y fwd-z _ :as forward] (svector/normalize (svector/sub to from))
        [left-x left-y left-z _ :as left] (svector/cross forward (svector/normalize up))
        [true-up-x true-up-y true-up-z _ :as true-up] (svector/cross left forward)]
    (matrix/mul4 (vector    left-x    left-y    left-z 0
                         true-up-x true-up-y true-up-z 0
                         (- fwd-x) (- fwd-y) (- fwd-z) 0
                                 0         0         0 1)
                 (transform/translate (- from-x)
                                      (- from-y)
                                      (- from-z)))))
