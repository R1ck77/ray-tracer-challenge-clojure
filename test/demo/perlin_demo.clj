(ns demo.perlin-demo
  (:require [raytracer.perlin :as perlin]
            [raytracer.canvas :as canvas]))

(def grid-rows 200)
(def grid-columns 320)

(defn render-demo
  ([] (render-demo 320 200))
  ([width height]
   (let [perlin-data {:grid (perlin/create-grid grid-rows grid-columns)
                      :x-scale grid-columns
                      :y-scale grid-rows}]
     (let [canvas (reduce (fn color-to-canvas [canvas [color [x y]]]
                     (canvas/write canvas x y color))
                   (canvas/create-canvas width height)
                   (map
                    (fn noise-to-color [[x y noise]]
                      (let [value (- 1 noise)]
                        (vector [value value value] (vector x y))))
                    (map
                     (fn pixel-to-noise [[y x]]
                       (vector x y (perlin/noise perlin-data [(/ x width) (/ y height)])))
                     (for [i (range height)
                           j (range width)]
                       (vector i j)))))]
      (spit "noise.ppm" (canvas/canvas-to-ppm canvas))))))
