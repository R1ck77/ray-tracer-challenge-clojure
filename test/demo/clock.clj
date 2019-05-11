(ns demo.clock
  (:require [raytracer.svector :as svector]
            [raytracer.color :as color]
            [raytracer.point :as point]
            [raytracer.canvas :as canvas]
            [raytracer.matrix :as matrix]            
            [raytracer.transform :as transform]))


(def canvas-size [40 30])

(def clock-radius 10)

(defn clock-position [angle-rad]
;;; TODO/FIXME some multiplications could be cached. Whatever
  (matrix/transform (transform/translate (/ (first canvas-size) 2)
                                         (/ (second canvas-size) 2)
                                         0
                                         (transform/scale -1 -1 1
                                                          (transform/rotate-z angle-rad
                                                                              (transform/translate 0 clock-radius 0))))
                    (point/point 0 0 0)))

(defn clock-angles-seq []
  (let [slice (/ Math/PI 6)]
    (map #(* slice %) (range 12))))

(defn compute-clock-positions []
  (map clock-position (clock-angles-seq)))

(defn render-demo []
  (spit "sad_clock.ppm"
        (canvas/canvas-to-ppm (reduce (fn [canvas point]
                                        (let [x (:x point)
                                              y (:y point)]
                                         (canvas/write canvas (int x) (int y) (color/color 1 1 1))))
                                      (apply canvas/create-canvas canvas-size)          
                                      (compute-clock-positions)))))
