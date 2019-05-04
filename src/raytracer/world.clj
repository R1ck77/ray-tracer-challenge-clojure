(ns raytracer.world
  (:require [raytracer.tuple :as tuple]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.color :as color]
            [raytracer.transform :as transform]
            [raytracer.ray :as ray]
            [raytracer.shapes :as shapes]
            [raytracer.intersection :as intersection]
            [raytracer.matrix :as matrix]
            [raytracer.materials :as materials]
            [raytracer.light-sources :as light-sources]
            [raytracer.phong :as phong]))

(def ^:dynamic *maximum-reflections* 100)

(def EPSILON 1e-6)

(defn create [] ;;; TODO/FIXME change name to "world"
  {:objects []
   :light-sources #{}
   :material materials/void-material})

(defn add-object [world object]
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

;;; TODO/FIXME not sure about this reduced stuff. Smells like cheating
(defn- compute-refraction-indices [checked-intersection intersections world-refractive-index]
  ;;; TODO/FIXME bogus return value
  (let [checked-intersection-object (-> checked-intersection :object :material) ;;; Horribly complex
        transitions (first (reduce (fn [[transitions inside] [checked-intersection object]]
                                     (let [[new-transitions new-inside :as status] (if (contains? inside object)
                                                                                     [(conj transitions [:out object])
                                                                                      (disj inside object)]
                                                                                     [(conj transitions [:in object])
                                                                                      (conj inside object)])]
                                       (if checked-intersection
                                         (reduced (if (empty? new-inside)
                                                    [(conj transitions [:in :void])
                                                     (conj inside :void)]
                                                    status))                                         
                                         status)))
                                   ['([:in :void]) #{:void}]
                                   (map (fn [intersection]
                                          (vector (= intersection checked-intersection) (:object intersection)))
                                        intersections)))]
    (map (fn to-refractive-index [[_ object-or-void]]
           (if (= :void object-or-void)
             world-refractive-index
             (-> object-or-void :material :refractive-index)))
         (reverse transitions))))

(defn- is-inside? [eye-v normal-v]
  (< (svector/dot eye-v normal-v) 0))

(defn prepare-computations
  [ray intersection all-intersections world-refractive-index]
  (let [point (tuple/add (:origin ray)
                         (svector/mul (:direction ray)
                                      (:t intersection)))
        eye-v (svector/neg (:direction ray))
        object (:object intersection)
        normal-v ((:normal object) object point)
        inside (is-inside? eye-v normal-v)
        normal-v (if inside (svector/neg normal-v) normal-v)
        [n1 n2] (compute-refraction-indices intersection all-intersections world-refractive-index)]
    {:inside inside
     :object object
     :t (:t intersection)
     :point  point
     :over-point (tuple/add point (svector/mul normal-v EPSILON))
     :eye-v eye-v
     :normal-v normal-v
     :reflection (svector/reflect (:direction ray) normal-v)
     :n1 n1
     :n2 n2}))

(defn- get-reflectivity [intermediate-result]
  (:reflectivity (:material (:object intermediate-result))))

(defn is-shadowed? [world point]
  (let [light-source (first (:light-sources world)) ;;; first light source only
        pos->light (tuple/sub (:position light-source) point)
        intersection (intersection/hit (intersect world (ray/ray point (svector/normalize pos->light))))]
    (and intersection
         (< (:t intersection)
            (svector/mag pos->light)))))

(def reflected-color)

;; TODO/FIXME the rendering throws without a light source set!
(defn shade-hit
  [world intermediate-result remaining]
  (let [shadowed (is-shadowed? world (:over-point intermediate-result))]
    (color/add (phong/lighting (:object intermediate-result)
                               (first (:light-sources world)) ;;; first light source, for now
                               (:point intermediate-result)
                               (:eye-v intermediate-result)
                               (:normal-v intermediate-result)
                               shadowed)
               (reflected-color world intermediate-result remaining))))

(defn color-at
  ([world ray]
   (color-at world ray *maximum-reflections*))
  ([world ray remaining]
   (let [intersections (intersect world ray)
         intersection (intersection/hit intersections)]
     (if intersection
       (shade-hit world (prepare-computations ray
                                              intersection
                                              intersections
                                              (-> world :material :refractive-index))
                  remaining)
       (-> world :material :color)))))


(defn reflected-color [world intermediate-result remaining]
  (if (> remaining 0)
   (let [reflectivity (get-reflectivity intermediate-result)]
     (if (< reflectivity EPSILON)
       (-> world :material :color)
       (let [reflection (ray/ray (:over-point intermediate-result)
                                 (:reflection intermediate-result))]
         (color/scale (color-at world reflection (dec remaining)) reflectivity))))
   (-> world :material :color)))

(defn view-transform [[from-x from-y from-z _ :as from] to up]
  (let [[fwd-x fwd-y fwd-z _ :as forward] (svector/normalize (svector/sub to from))
        [left-x left-y left-z _ :as left] (svector/cross forward (svector/normalize up))
        [true-up-x true-up-y true-up-z _ :as true-up] (svector/cross left forward)]
    (matrix/mul4 (vector left-x      left-y      left-z      0
                         true-up-x   true-up-y   true-up-z   0
                         (- fwd-x)   (- fwd-y)   (- fwd-z)   0
                         0           0           0           1)
                 (transform/translate (- from-x)
                                      (- from-y)
                                      (- from-z)))))
