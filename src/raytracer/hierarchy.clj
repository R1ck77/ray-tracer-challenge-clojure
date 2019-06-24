(ns raytracer.hierarchy
  (:require [clojure.zip :as zip]
            [raytracer.shapes.group :as group]))

(defprotocol CoordinatesConverter
  (to-world-coordinates [this shape point]))

(defrecord Hierarchy [zipper]
  CoordinatesConverter
  (to-world-coordinates [this shape point]))


(defn- create-zipper [root]
  (zip/zipper )
  )


(defn hierarchy [root]
  (create-zipper root))
