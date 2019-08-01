(ns raytracer.world
  (:require [raytracer.const :as const]
            [raytracer.tuple :as tuple]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.color :as color]
            [raytracer.transform :as transform]
            [raytracer.ray :as ray]
            [raytracer.shapes :as shapes]
            [raytracer.shapes.group :as group]
            [raytracer.shapes.shared :as shared]
            [raytracer.intersection :as intersection]
            [raytracer.matrix :as matrix]
            [raytracer.material :as material]
            [raytracer.light-sources :as light-sources]
            [raytracer.refraction :as refraction]
            [raytracer.phong :as phong]
            [raytracer.grouping.hierarchy :as hierarchy]
            [raytracer.grouping.shared :as grouping-shared]))

(def ^:dynamic *maximum-reflections* 4)
(def ^:dynamic *basic-shade-detection* true)

(def zero-color (color/color 0 0 0))

(defn world
  ([]
   (world []))
  ([root-objects]
   {:hierarchy (hierarchy/hierarchy (group/group root-objects))
    :light-sources #{}
    :material material/void-material}))

(defn add-root-object
  "Add a new object to the root group"
  [world object]
  (let [old-hierarchy (:hierarchy world)]
    (assoc world :hierarchy (grouping-shared/add-root-object old-hierarchy object))))

(defn set-objects [world objects]
  (assoc world :hierarchy (hierarchy/hierarchy (group/group objects))))

(defn get-objects [world]
  (grouping-shared/get-all-objects (:hierarchy world)))

(defn set-light-sources [world & light-sources]
  (assoc world :light-sources (vec light-sources)))

(defn default-world
  "A world used for testing.

  It kind of sucks conceptually that this function is here,
  but at least at the moment I'll keep it here because
  \"The Book made me do it.\""
  []
  (let [sphere1 (assoc (shapes/change-material (shapes/sphere)
                                         (material/with-color (color/color 0.8 1.0 0.6)
                                           :diffuse 0.7
                                           :specular 0.2))
                       :name :shape1)
        sphere2 (assoc (shared/transform (shapes/sphere)
                                         (transform/scale 0.5 0.5 0.5))
                       :name :shape2)]
    (-> (world [sphere1 sphere2])
        (set-light-sources (light-sources/create-point-light (point/point -10 10 -10)
                                                             (color/color 1 1 1))))))

(defn- unsorted-intersections [world ray]
  (ray/intersect ray (grouping-shared/get-root (:hierarchy world)))) ;; TODO/FIXME bogus

(defn intersect [world ray]
  (sort-by :t (unsorted-intersections world ray)))

(defn- is-inside? [eye-v normal-v]
  (< (tuple/dot eye-v normal-v) 0))

(defn- compute-surface-parameters [hierarchy ray object point eye-v]
  (let [basic-normal-v (shared/compute-normal object point)
        inside (is-inside? eye-v basic-normal-v)
        normal-v (if inside (tuple/neg basic-normal-v) basic-normal-v)]
    {:inside inside     
     :normal-v normal-v
     :over-point (tuple/add point (tuple/mul normal-v const/EPSILON))
     :under-point (tuple/add point (tuple/mul normal-v (- const/EPSILON)))
     :reflection (tuple/reflect (:direction ray) normal-v)}))

(defn prepare-computations
  [hierarchy ray intersection refractive-indices]
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
           (compute-surface-parameters hierarchy ray object point eye-v))))

(defn- filtered-transparencies [intersections light-distance]
  (map #(-> % :object :material :transparency)
       (filter (fn [{t :t}]
                 (and (< t light-distance)
                      (> t 0)))
               intersections)))

;;; TODO/FIXME consider a more physics based shadowing model based on the distance traveled inside the material
(defn- compute-shadow-attenuation
  "Slightly more complex shadow computation
  
  This function is an improvement over the basic shadowed/lit model in
  the book, altough still very crude.

  It computes the transparency of each object the light goes through
  when moving from the point to the light, and picks the one with the
  lowest transparency as the attenuation to use.

  A better model could compute the attenuation through each object to
  account for multiple semi-transparent objects.

  Also keep in mind that this transparency model is not physics based,
  as the light attenuation doesn't account the length the light goes
  through the material. Using intersection points to compute how deep
  an object goes into a material could make an interesting upgrade."
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
    (if (basic-is-shadowed? world point)
      0.0
      1.0)
    (compute-shadow-attenuation world point)))

(def reflected-color)
(def refracted-color)

(defn combine-colors
  [intermediate-result surface reflected refracted]
  (let [ material (-> intermediate-result :object :material)]
    (if (and (> (:transparency material) 0)
             (> (:reflectivity material) 0))
      (let [reflectance (refraction/schlick intermediate-result)]
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
       (shade-hit world (prepare-computations (:hierarchy world)
                                              ray
                                              intersection
                                              (refraction/compute-refractive-indices intersection
                                                                                     intersections
                                                                                     (-> world :material :refractive-index)))
                  remaining)
       (-> world :material material/get-color)))))

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
    (-> world :material material/get-color)))

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
    (matrix/mul4 (matrix/create
                  (vector (:x left)      (:y left)      (:z left)      0
                          (:x true-up)   (:y true-up)   (:z true-up)   0
                          (- (:x forward))   (- (:y forward))   (- (:z forward))   0
                          0           0           0           1))
                 (transform/translate (- (:x from))
                                      (- (:y from))
                                      (- (:z from))))))
