(ns raytracer.canvas)

(def max-value 255)

(defn create-canvas [width height]
  {:width width
   :height height
   :pixels (vec (repeat (* width height) [0 0 0]))})

(defn- index [width x y]
  (+ x (* y width)))

(defn read [canvas x y]
  (nth (:pixels canvas) (index (:width canvas) x y)))

(defn write [canvas x y color]
  ;;; TODO/FIXME this is gonna cost me *a lot* :)
  (update canvas :pixels
          (fn [pixels]
            (assoc pixels (index (:width canvas) x y) color))))

(defn- color-to-ppm [component]
  (cond
      (<= component 0) 0
      (>= component 1) max-value
      :default (Math/round (* component max-value))))

(defn- format-color [color]
  (map color-to-ppm color))

(defn- format-line [colors]
  (str (apply str (interpose " " (mapcat format-color colors))) "\n"))

(defn- format-pixels [colors width]
  (mapcat format-line (partition width colors)))

(defn canvas-to-ppm [canvas]
  (let [width (:width canvas)]
    (str (apply str (interpose "\n"
                           (concat (vector "P3"
                                           (str width " " (:height canvas))
                                           max-value))))
         "\n"
         (apply str (format-pixels (:pixels canvas) width)))))
