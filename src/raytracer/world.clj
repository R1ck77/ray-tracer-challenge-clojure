(ns raytracer.world
  (:require [raytracer.const :as const]
            [raytracer.tuple :as tuple]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.color :as color]
            [raytracer.transform :as transform]
            [raytracer.ray :as ray]
            [raytracer.shapes :as shapes]
            [raytracer.shapes.shared :as shared]
            [raytracer.intersection :as intersection]
            [raytracer.matrix :as matrix]
            [raytracer.material :as material]
            [raytracer.light-sources :as light-sources]
            [raytracer.phong :as phong]))

(def ^:dynamic *maximum-reflections* 4)
(def ^:dynamic *basic-shade-detection* true)

(def zero-color (color/color 0 0 0))

(defn world []
  {:objects []
   :light-sources #{}
   :material material/void-material})

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
  (-> (world)
      (add-object (shapes/change-material (shapes/sphere)
                                          (material/material :color (color/color 0.8 1.0 0.6)
                                                              :diffuse 0.7
                                                              :specular 0.2)))
      (add-object (shapes/change-transform (shapes/sphere)
                                           (transform/scale 0.5 0.5 0.5)))    
      (add-light-source (light-sources/create-point-light (point/point -10 10 -10)
                                                          (color/color 1 1 1)))))

(defn- unsorted-intersections [world ray]
  (flatten
   (persistent!
    (reduce (fn [acc object]
              (conj! acc (ray/intersect ray object)))
            (transient [])
            (:objects world)))))

(defn intersect [world ray]
  (sort-by :t (unsorted-intersections world ray)))

;;; TODO/FIXME This bunch of refraction functions should probably deserve a namespace on their own
(defn- convert-to-refractive-index [object-or-void world-refractive-index]
  (if (= object-or-void :void)
    world-refractive-index
    (-> object-or-void :material :refractive-index)))

(defn- take-until-including [filter-f list]
  (persistent!
   (reduce (fn [collection x]
             (if (filter-f x)
               (conj! collection x)
               (reduced (conj! collection x))))
           (transient [])
           list)))

(defn- accumulate-transitions [intersections]
  (persistent! (reduce (fn [transitions intersection]
                         (conj! transitions (:object intersection)))
                       (transient [:void])
                       intersections)))

(defn- compute-transitions [intersections checked-intersection]
  (accumulate-transitions
   (take-until-including #(not= % checked-intersection)
                         intersections)))

(defn- get-first-odd-recurrence [xo frequencies]
  (first (drop-while #(even? (get frequencies %)) xo)))

(defn- compute-transition [transitions]
  (let [frequencies (frequencies transitions)
        reversed-list (reverse transitions)
        last-element (first reversed-list)]
    [(get-first-odd-recurrence (rest reversed-list)
                               (update frequencies (first reversed-list) #(dec %)))
     (get-first-odd-recurrence reversed-list frequencies)]))

(defn compute-refractive-indices [checked-intersection intersections world-refractive-index]
  (let [objects-transitions (compute-transitions intersections checked-intersection)
        [obj1 obj2] (compute-transition objects-transitions)]
    {:n1 (convert-to-refractive-index obj1 world-refractive-index)
     :n2 (convert-to-refractive-index obj2 world-refractive-index)}))

(defn- schlick-partial-reflection [n1 n2 cos]
  (let [r0 (Math/pow (/ (- n1 n2) (+ n1 n2)) 2)]
    (+ r0 (* (- 1 r0) (Math/pow (- 1 cos) 5)))))

(defn- schlick-total-internal-reflection [n1 n2 cos]
  (let [n (/ n1 n2)
        sin2-t (* n n (- 1 (* cos cos)))]
    (if (> sin2-t 1)
      1
      (schlick-partial-reflection n1 n2 (Math/sqrt (- 1 sin2-t))))))

(defn schlick [{:keys [n1 n2 eye-v normal-v]}]
  (let [cos (tuple/dot eye-v normal-v)]
    (if (> n1 n2)
      (schlick-total-internal-reflection n1 n2 cos)
      (schlick-partial-reflection n1 n2 cos))))

(defn- is-inside? [eye-v normal-v]
  (< (tuple/dot eye-v normal-v) 0))

(defn- compute-surface-parameters [ray object point eye-v]
  (let [basic-normal-v (shared/compute-normal object point)
        inside (is-inside? eye-v basic-normal-v)
        normal-v (if inside (tuple/neg basic-normal-v) basic-normal-v)]
    {:inside inside     
     :normal-v normal-v
     :over-point (tuple/add point (tuple/mul normal-v const/EPSILON))
     :under-point (tuple/add point (tuple/mul normal-v (- const/EPSILON)))
     :reflection (tuple/reflect (:direction ray) normal-v)}))

(defn prepare-computations
  [ray intersection refractive-indices]
  (let [object (:object intersection)
        ray-direction (:direction ray)
        point (tuple/add (:origin ray)
                         (tuple/mul ray-direction
                                    (:t intersection)))
        eye-v (tuple/neg ray-direction)]
    (merge {:object object
            :point point
            :eye-v eye-v
            :t (:t intersection)}
           refractive-indices
           (compute-surface-parameters ray object point eye-v))))

(defn- filtered-transparencies [intersections light-distance]
  (map #(-> % :object :material :transparency)
       (filter (fn [{t :t}]
                 (and (< t light-distance)
                      (> t 0)))
               intersections)))

;;; TODO/FIXME this function is oversimplifying the model, even after the update to account transparency
;;; There is no way to filter a light through a colored glass (or multiple ones!)
;;; While simulating transparency for arbitrary objects with some realism would be complicated, you can at least:
;;; a) get the list of intersections with their transparencies
;;; b) remove the concept of shadow and just filter the light through all objects the light passes through
(defn- compute-shadow-attenuation
  [world point]
  (let [light-source (first (:light-sources world)) ;;; first light source only
        pos->light (tuple/sub (:position light-source) point)
        transparencies (filtered-transparencies (intersect world (ray/ray point (tuple/normalize pos->light)))
                                                (tuple/mag pos->light))]
    (apply * (conj transparencies 1))))

(defn- basic-is-shadowed?
  [world point]
  (let [light-source (first (:light-sources world)) ;;; first light source only
        pos->light (tuple/sub (:position light-source) point)
        intersection (intersection/hit (filter #(< (:t %)
                                                   (tuple/mag pos->light))
                                               (intersect world (ray/ray point (tuple/normalize pos->light)))))]
    intersection))

(defn select-shadow-attenuation
  [world point]
  (if *basic-shade-detection*
    (if (basic-is-shadowed? world point) 0.0 1.0)
    (compute-shadow-attenuation world point)))

(def reflected-color)
(def refracted-color)

(defn combine-colors
  [intermediate-result surface reflected refracted]
  (let [ material (-> intermediate-result :object :material)]
    (if (and (> (:transparency material) 0)
             (> (:reflectivity material) 0))
      (let [reflectance (schlick intermediate-result)]
        (-> surface
            (color/add (color/scale reflected reflectance))
            (color/add (color/scale refracted (- 1 reflectance)))))      
      (-> surface
          (color/add reflected)
          (color/add refracted)))))

(defn shade-hit
  [world intermediate-result remaining]
  (let [shadow-attenuation (select-shadow-attenuation world (:over-point intermediate-result))
        surface (phong/lighting (:object intermediate-result)
                                (first (:light-sources world)) ;;; first light source, for now
                                (:point intermediate-result)
                                (:eye-v intermediate-result)
                                (:normal-v intermediate-result)
                                shadow-attenuation)
        reflected (reflected-color world intermediate-result remaining)
        refracted (refracted-color world intermediate-result remaining)]
    (combine-colors intermediate-result surface reflected refracted)))

(defn color-at
  ([world ray]
   (color-at world ray *maximum-reflections*))
  ([world ray remaining]
   (let [intersections (intersect world ray)
         intersection (intersection/hit intersections)]
     (if intersection
       (shade-hit world (prepare-computations ray
                                              intersection
                                              (compute-refractive-indices intersection
                                                                          intersections
                                                                          (-> world :material :refractive-index)))
                  remaining)
       (-> world :material :color)))))

(defn- get-reflectivity [intermediate-result]
  (:reflectivity (:material (:object intermediate-result))))

(defn reflected-color [world intermediate-result remaining]
  (if (> remaining 0)
    (let [reflectivity (get-reflectivity intermediate-result)]
      (if (< reflectivity const/EPSILON)
        zero-color
        (let [reflection (ray/ray (:over-point intermediate-result)
                                  (:reflection intermediate-result))]
          (color/scale (color-at world reflection (dec remaining)) reflectivity))))
    (-> world :material :color)))

(defn- compute-refracted-ray [intermediate-result n-ratio sin-t-squared cos-i]
  (let [cos-t (Math/sqrt (- 1 sin-t-squared))]
    (ray/ray (:under-point intermediate-result)
             (tuple/sub (tuple/mul (:normal-v intermediate-result)
                                   (- (* n-ratio cos-i) cos-t))
                        (tuple/mul (:eye-v intermediate-result) n-ratio)))))

(defn- refraction-color [world intermediate-result remaining]
  (let [n-ratio (/ (:n1 intermediate-result)
                   (:n2 intermediate-result))
        cos-i (tuple/dot (:eye-v intermediate-result)
                         (:normal-v intermediate-result))
        sin-t-squared (* n-ratio n-ratio (- 1 (* cos-i cos-i)))]
    (if (> sin-t-squared 1)
      (color/color 0 0 0)
      (color/scale (color-at world
                             (compute-refracted-ray intermediate-result n-ratio sin-t-squared cos-i)
                             (dec remaining))
                   (-> intermediate-result :object :material :transparency)))))

(defn- get-transparency [intermediate-result]
  (-> intermediate-result :object :material :transparency))

(defn refracted-color [world intermediate-result remaining]
  (if (> remaining 0)
    (let [transparency (get-transparency intermediate-result)]
      (if (< transparency const/EPSILON)
        zero-color
        (refraction-color world intermediate-result remaining)))
    zero-color))

(defn view-transform [from to up]
  (let [forward (tuple/normalize (tuple/sub to from))
        left (tuple/cross forward (tuple/normalize up))
        true-up (tuple/cross left forward)]
    (matrix/mul4 (double-array
                  (vector (:x left)      (:y left)      (:z left)      0
                          (:x true-up)   (:y true-up)   (:z true-up)   0
                          (- (:x forward))   (- (:y forward))   (- (:z forward))   0
                          0           0           0           1))
                 (transform/translate (- (:x from))
                                      (- (:y from))
                                      (- (:z from))))))
