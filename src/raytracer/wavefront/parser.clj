(ns raytracer.wavefront.parser
  (:require [clojure.string :as string]
            [raytracer.point :as point]))

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
              (conj vertices (apply point/point (mapv #(Double/valueOf (str %)) (rest tokens))))))))

(defmethod parse-tokens :default
  [acc line]
  (update acc
          :ignored
          #(conj % line)))

(defn parse [text]
  (reduce parse-tokens
          {:ignored []
           :vertices [nil]}
          (string/split-lines text)))
