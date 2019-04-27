(ns demo.perlin-demo
  (:require [raytracer.perlin :as perlin]
            [raytracer.canvas :as canvas]))

(def grid-width 20)
(def grid-height 20)

(defn write-color-to-canvas [width height colors]
  (reduce (fn color-to-canvas [canvas [color [x y]]]
            (canvas/write canvas x y color))
          (canvas/create-canvas width height)
          colors))

(defn convert-noise-to-color [noise]
  (map
   (fn noise-to-color [[x y noise]]
                      (let [value (/ (+ 1 noise) 2)]
                        (vector [value value value] (vector x y))))
   noise))

(defn create-noise [perlin-data width height]
  (pmap (fn pixel-to-noise [[x y]]
         (vector x y (perlin/noise perlin-data [(- (/ x width) 0.5) (- (/ y height) 0.5)])))
       (for [i (range height)
             j (range width)]
         (vector j i))))

(defn render-demo
  ([] (render-demo 400 400))
  ([width height]
   (let [perlin-data (perlin/create-perlin-data grid-width grid-height)]
     (spit "noise.ppm" (canvas/canvas-to-ppm
                        (write-color-to-canvas width height
                                               (convert-noise-to-color           
                                                (create-noise perlin-data width height))))))))
