(ns demo.perlin-demo
  (:require [raytracer.perlin :as perlin]
            [raytracer.canvas :as canvas]))

(def grid-width 320)
(def grid-height 200)

(defn render-demo
  ([] (render-demo 320 200))
  ([width height]
   (let [perlin-data (perlin/create-perlin-data grid-width grid-height)]
     (let [canvas (reduce (fn color-to-canvas [canvas [color [x y]]]
                     (canvas/write canvas x y color))
                   (canvas/create-canvas width height)
                   (map
                    (fn noise-to-color [[x y noise]]
                      (let [value (- 1 noise)]
                        (vector [value value value] (vector x y))))
                    (map
                     (fn pixel-to-noise [[x y]]
                       (vector x y (perlin/noise perlin-data [(/ x width) (/ y height)])))
                     (for [i (range height)
                           j (range width)]
                       (vector j i)))))]
      (spit "noise.ppm" (canvas/canvas-to-ppm canvas))))))
