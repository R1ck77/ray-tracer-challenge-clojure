(ns raytracer.utils
  (:require [raytracer.const :as const]))

(defn zero? [v]
  (< (Math/abs (double v)) const/EPSILON))

(defmacro mmap [f v]
  `(vector
    ~@(map #(list f %) v)))


(defn template1 [f [a]]
  (if (f a)
    [a]
    []))

(defn template2 [f [a b]]
  (if (f a)
    (if (f b)
      [a b]
      [a])
    (if (f b)
      [b]
      [])))


(defn template3 [f [a b c]]
  (if (f a)
    (if (f b)
      (if (f c)
        [a b c]
        [a b])
      (if (f c)
        [a c]
        [a]))
    (if (f b)
      (if (f c)
        [b c]
        [b])
      (if (f c)
        [c]
        []))))

;;; [A] (A [a] [!a])
;;; 
;;; [A B] (A (B [a b] [a)
;;;          (B [b] []))
;;; 
;;; [A B C] (A (B (C [a b c] [a b])
;;;               (C [a c] [a]))
;;;            (B (C [b c] [b])
;;;               (C [c] [])))


(defn templ
  ([values]
   (templ values []))
  ([values carried]
   (let [next (first values)
         remaining (rest values)]
     (if (empty? remaining)
       ;;; last one
       (vector (str next)
               (conj carried next)
               carried)
       ;;; more to come
       (vector (str next)
               (templ remaining (conj carried next))
               (templ remaining carried))
       )
     )))

