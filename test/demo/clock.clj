(ns demo.clock
  (:require [raytracer.svector :refer :all]
            [raytracer.point :refer :all]
            [raytracer.canvas :refer :all]
            [raytracer.matrix :refer :all]            
            [raytracer.transform :refer :all]))


(def canvas-size [800 600])

(def clock-radius 250)

(defn clock-position [angle-rad]
  ;;; TODO/FIXME some multiplications could be cached. Whatever
  (transform (translate 400 300 0
                        (scale -1 -1 1
                               (rotate-z angle-rad
                                         (translate 0 clock-radius 0))))
             (point 0 0 0)))

(defn clock-angles-seq []
  (let [slice (/ Math/PI 6)]
    (map #(* slice %) (range 12))))

(defn compute-clock-positions []
  (map clock-position (clock-angles-seq)))

(defn draw-clock []
  (spit "sad_clock.ppm"
        (canvas-to-ppm (reduce (fn [canvas [x y _ _]]
                                 (write canvas (int x) (int y) [1 1 1]))
                               (apply create-canvas canvas-size)          
                               (compute-clock-positions)))))

