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

(defmethod parse-tokens "f"
  [acc line]
  (let [tokens (tokens line)
        group (:current-group acc)
        new-face (apply triangle/triangle
                        (map #(get (:vertices acc) %)
                             ;;; this "str" is bad
                             (map #(Integer/valueOf (str %)) (rest tokens))))]
    (update-in acc
               [:groups group]
               (fn [faces]
                 (conj faces new-face)))))

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
