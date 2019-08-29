(ns raytracer.wavefront.parser
  (:require [clojure.string :as string]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.shapes.triangle :as triangle]
            [raytracer.shapes.smooth-triangle :as smooth-triangle]))

(defn- tokens [line]
  (string/split line #"[ ]+" ))

(defn- first-token [line]
  (apply str (take-while #(not (= \space %)) line)))

(defn- parse-tokens-dispatch [result line]
  (first-token line))

(defmulti parse-tokens #'parse-tokens-dispatch)

(defn- read-tokens-with-function [tokens obj-conv-f vertices]
  (conj vertices
        (apply obj-conv-f (mapv #(Double/valueOf ^String %) (rest tokens)))))

(defn read-vertex-data [acc line key obj-conv-f]
  (update acc key (partial read-tokens-with-function (tokens line) obj-conv-f)))

(defmethod parse-tokens "v"
  [acc line]
  (read-vertex-data acc line :vertices point/point))

(defmethod parse-tokens "vn"
  [acc line]
  (read-vertex-data acc line :normals svector/svector))

(defn- create-smooth-triangle [vertices normals]
  (apply smooth-triangle/smooth-triangle (concat vertices normals)))

(defn- create-triangle [vertices _]
  (apply triangle/triangle vertices))

(defn- triangles-from-vertices
  [grouped-vertices grouped-normals]
  (mapv #(if %2
           (create-smooth-triangle % %2)
           (create-triangle % %2))
        grouped-vertices grouped-normals))

(defn- extract-vertex-indices [triangle-indices]
  (map first triangle-indices))

(defn- convert-to-vertices-for-single-triangle [all-vertices indices-group]
  (map #(get all-vertices %)
       indices-group))

(defn- indices-to-points [all-vertices grouped-indices]
  (let [vertex-indices (map extract-vertex-indices grouped-indices)]
    (map (partial convert-to-vertices-for-single-triangle all-vertices) vertex-indices)))

(defn- normals-present? [normal-indices]
  (not (some nil? normal-indices)))

(defn- extract-normal-indices [triangle-indices]
  (map #(nth % 2) triangle-indices))

(defn- convert-to-normals-for-single-triangle [all-normals triangle-normal-indices]
  (when (normals-present? triangle-normal-indices)
    (map #(get all-normals %)
         triangle-normal-indices)))

(defn- indices-to-normals [all-normals grouped-indices]
  (let [normal-indices (map extract-normal-indices grouped-indices)]
    (map (partial convert-to-normals-for-single-triangle all-normals) normal-indices)))

(defn- group-vertex-elements
  "Given N elements, return a list with 3-tuples corresponding to a triangle fan decomposition.

  Works only for convex figures"
  [xn]
  (let [start (first xn)]
    (mapv #(conj % start)
          (partition 2 1 (rest xn) ))))

(defn- pad-token
  "Return a 3-tuple by possibly padding the input"
  [tuple]
  (vec
   (case (count tuple)
     3 tuple
     2 (concat tuple [nil])
     1 (concat tuple [nil nil])
     (throw (IllegalArgumentException. "empty tuples not expected!")))))

(defn- read-token
  "Read a numeric token as an int, return nil for empty strings"
  [token]
  (if (empty? token)
    nil
    (Integer/valueOf ^String token)))

(defn- parse-value-token
  "Receives a strings and returns a list strings/nil.

  e.g.
  \"1\" returns [1 nil nil]
  \"1/2/3\" returns [1 2 3]
  \"1//5\" returns [1 nil 5]"
  [string]
  (pad-token (map read-token (string/split string #"/"))))

(defn- add-faces [faces new-faces]
  (vec
   (concat faces new-faces)))

(defmethod parse-tokens "f"
  [acc line]
  (let [tokens (tokens line)
        group (:current-group acc)
        grouped-tokens (group-vertex-elements
                        (map parse-value-token (rest tokens)))        
        new-faces (triangles-from-vertices
                   (indices-to-points (:vertices acc) grouped-tokens)
                   (indices-to-normals (:normals acc) grouped-tokens))]
    (update-in acc
               [:groups group]
               #(add-faces % new-faces))))

(defmethod parse-tokens "g"
  [acc line]
  (let [tokens (tokens line)
        new-group (first (rest tokens))
        updated-acc (assoc acc :current-group new-group)]
    (if-not (-> acc :groups (get new-group))
      (assoc-in updated-acc [:groups new-group] [nil])
      updated-acc)))

(defmethod parse-tokens :default
  [acc line]
  (update acc
          :ignored
          #(conj % line)))

(defn parse [text]
  (reduce parse-tokens
          {:current-group :default-group
           :ignored []
           :vertices [nil]
           :normals [nil]
           :groups {:default-group [nil]}}
          (string/split-lines text)))

(defn get-non-empty-groups [parsing-result]
  (filter (complement empty?)
          (map rest
               (map second
                    (:groups parsing-result)))))
