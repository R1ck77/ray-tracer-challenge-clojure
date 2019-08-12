(ns raytracer.wavefront.parser
  (:require [clojure.string :as string]
            [raytracer.point :as point]
            [raytracer.shapes.triangle :as triangle]))

(defn- tokens [line]
  (string/split line #"[ ]+" ))

(defn- first-token [line]
  (apply str (take-while #(not (= \space %)) line)))

(defn- parse-tokens-dispatch [result line]
  (first-token line))

(defmulti parse-tokens #'parse-tokens-dispatch)

(defmethod parse-tokens "v"
  [acc line]
  (let [tokens (tokens line)]
    (update acc
            :vertices
            (fn [vertices]
              ;;; that "str" is bad
              (conj vertices (apply point/point (mapv #(Double/valueOf (str %)) (rest tokens))))))))

(defn- triangles-from-vertices [grouped-vertices]
  (mapv #(apply triangle/triangle %) grouped-vertices))

(defn- indices-to-points [all-vertices grouped-indices]
  (map (fn [indices-group]
         (map #(get all-vertices %) indices-group)) grouped-indices))

(defn- group-vertex-indices
  "Given N vertices, return a list of triangle vertices

  Uses the fan decomposition of a polygon, which works only for convex figures"
  [xn]
  (let [start (first xn)]
    (mapv #(conj % start)
          (partition 2 1 (rest xn) ))))

(defn int-tokens [strings]
  (map #(Integer/valueOf (str %)) strings))

(defmethod parse-tokens "f"
  [acc line]
  (let [tokens (tokens line)
        group (:current-group acc)
        new-faces (triangles-from-vertices
                   (indices-to-points (:vertices acc)
                    (group-vertex-indices
                     (int-tokens (rest tokens)))))]
    (update-in acc
               [:groups group]
               (fn [faces]
                 (vec
                  (concat faces new-faces))))))

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
           :groups {:default-group [nil]}}
          (string/split-lines text)))

(defn get-non-empty-groups [parsing-result]
  (filter (complement empty?)
          (map rest
               (map second
                    (:groups parsing-result))))) ;;; TODO/FIXME test this!

