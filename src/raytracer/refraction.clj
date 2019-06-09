(ns raytracer.refraction
  (:require [raytracer.tuple :as tuple]))

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

