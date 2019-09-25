(ns raytracer.ray
  (:require [raytracer.intersection :as intersection]
            [raytracer.shapes.shared :as shared]
            [raytracer.utils :as utils]
            [raytracer.tuple :as tuple]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]
            [raytracer.material :as material]
            [raytracer.shapes.placement :as placement]
            [raytracer.shapes.bounding-box :as bounding-box]))

(def ^:dynamic *use-bounding-boxes* true)
(def ^:dynamic *statistics* false) ; whether sampling the number of aabb hits or not

(def hit-count-statistics (atom [0 0])) ; used for debugging statistics only

(defn- update-statistics
  "Only used to debug the aabb statistics"
  [hit]
  (if *statistics*
    (swap! hit-count-statistics
           #(vector (if hit (inc (first %)) (first %))
                    (inc (second %)))))
  hit)

(defprotocol RayCaster
  (intersect [this direction]))

(defrecord Ray [origin direction])

(defn ray [origin shape]
  (->Ray origin shape))

(defn transform [input-ray matrix]
  (ray (matrix/transform matrix (:origin input-ray))
       (matrix/transform matrix (:direction input-ray))))

(defn- intersects-bounding-box? [ray shape]
  (update-statistics
   (if *use-bounding-boxes*
     (bounding-box/hit (shared/get-bounding-box shape)
                       ray)
     true)))

(defn- ray-intersection [ray shape]
  (if (intersects-bounding-box? ray shape)
    (shared/local-intersect shape (transform ray (-> shape shared/get-placement placement/get-inverse-transform)))
    []))

(extend-type Ray
  RayCaster
  (intersect [this shape]
    (ray-intersection this shape)))

(defn normalize [ray]
  (assoc ray
         :direction (tuple/normalize (:direction ray))))

(defn position [ray t]
  (tuple/add (:origin ray)
             (tuple/mul (:direction ray) t)))
