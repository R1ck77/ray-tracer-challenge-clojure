(ns raytracer.shapes.parent)

(defprotocol Parent
  (get-children [this])
  (is-empty? [this])
  (set-children [this children]))
