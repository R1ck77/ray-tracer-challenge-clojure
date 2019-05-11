(ns raytracer.color
  (:require [raytracer.tuple :as tuple]))

(defrecord Color [r g b])

(defn color [r g b]
  (->Color r g b))

(defprotocol Algebra
  (add [color-1 color-2])
  (sub [color-1 color-2])
  (mul [color-1 color-2])
  (scale [color value]))

(def black (color 0 0 0))

(defmacro binary-operation [operator color-1 color-2]
  `(color (~operator (:r ~color-1) (:r ~color-2))
          (~operator (:g ~color-1) (:g ~color-2))
          (~operator (:b ~color-1) (:b ~color-2))))

(extend-type Color
  Algebra
  (add [this that]
    (binary-operation + this that))
  (sub [this that]
    (binary-operation - this that))
  (mul [this that]
    (binary-operation * this that))
  (scale [this value]
    (color (* (:r this) value)
           (* (:g this) value)
           (* (:b this) value))))
