(ns raytracer.tuple
  (:require [raytracer.const :as const]))

(defprotocol AboutComparable
  (aboutEqual [this tuple]))

(defprotocol Algebra
  (add [this that])
  (sub [this that])
  (neg [this])
  (mul [this value])
  (div [this value]))

(defprotocol Generic
  (op [this that operator]))

(defprotocol VectorOperations
  (mag [this])
  (normalize [this])
  (dot [this that])
  (cross [this that])
  (reflect [this normal]))

(defmacro binary-tuple-operation [operator op1 op2]
  (let [function# operator]
    `(->Tuple (~function# (:x ~op1) (:x ~op2))
              (~function# (:y ~op1) (:y ~op2))
              (~function# (:z ~op1) (:z ~op2))
              (~function# (:w ~op1) (:w ~op2)))))

(defmacro multi-binary-tuple-operation [operator op args]
  `(reduce #(binary-tuple-operation ~operator % %2) ~op ~args))

(defmacro unary-tuple-operation [f op]
  (let [function# f]
    `(->Tuple (~function# (:x ~op))
              (~function# (:y ~op))
              (~function# (:z ~op))
              (~function# (:w ~op)))))

(defrecord Tuple [x y z w]
  Algebra
  (add [this that]
    (binary-tuple-operation + this that))
  (sub [this that]
    (binary-tuple-operation - this that))
  (neg [this]
    (unary-tuple-operation #(- %) this))  
  (div [this value]
    (unary-tuple-operation #(/ % value) this))  
  (mul [this value]
    (unary-tuple-operation #(* % value) this))
  VectorOperations
  (dot [this that]
    (let [product (binary-tuple-operation * this that)]
      (+ (:x product)
         (:y product)
         (:z product)
         (:w product))))
  (mag [this]
    (let [squares (binary-tuple-operation #(* % %2) this this)]
      (Math/sqrt (float (+ (:x squares)
                           (:y squares)
                           (:z squares))))))
  (normalize [this]
    (let [length (.mag this)]
      (if (< length const/EPSILON)
        (->Tuple 0 0 0 0)
       (.div this length))))
  (cross [this that]
    (->Tuple (- (* (:y this) (:z that)) (* (:z this) (:y that)))
             (- (* (:z this) (:x that)) (* (:x this) (:z that)))
             (- (* (:x this) (:y that)) (* (:y this) (:x that)))
             0))
  (reflect [this normal]
    (.sub this (mul normal (* 2 (dot normal this)))))
  Generic
  (op [this that operator]
    (binary-tuple-operation operator this that)))

(defn tuple [x y z w]
  (->Tuple x y z w))

(defn add-all [args]
  (reduce #(add % %2)
          (tuple 0 0 0 0)
          args))
