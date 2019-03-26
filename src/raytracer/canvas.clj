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

(defn- all-spaces-in-line [line]
    (map first
         (filter #(= \space (second %))
                 (map-indexed vector line))))

(defn- find-split-index [line max-length]
  (first
   (reverse
    (take-while #(<= % max-length) (all-spaces-in-line line)))))

(defn- fix-line-length [line max-length]
  (let [index (find-split-index line max-length)]
    (filter (complement empty?) (list (subs line 0 index)
                                      (subs line (inc index) (count line))))))

(defn- needs-work? [lines max-length]
  (> (count (first lines)) max-length))

(defn- fix-first-element [inv-lines max-length]
  (concat
   (reverse (fix-line-length (first inv-lines)
                             max-length))
   (rest inv-lines) ))

(defn fix-length [lines max-length]
  (if (needs-work? lines max-length)
    (recur
     (fix-first-element lines max-length)
     max-length)
    (reverse lines)))

(defn- correct-line-sizes
  ([lines]
   (correct-line-sizes lines 70))
  ([lines max-length]
   (mapcat #(fix-length (vector %) max-length) lines)))

(defn- color-to-ppm [component]
  (cond
      (<= component 0) 0
      (>= component 1) max-value
      :default (Math/round (* component max-value))))

(defn- format-color [color]
  (map color-to-ppm color))

(defn- format-line [colors]
  (apply str (interpose " " (mapcat format-color colors))))

(defn- format-pixels [colors width]
  (map format-line (partition width colors)))

(defn- format-lines [canvas]
  (correct-line-sizes (format-pixels (:pixels canvas) (:width canvas))))

(defn create-header-lines [canvas]
  (vector "P3"
          (str (:width canvas) " " (:height canvas))
          max-value))

(defn create-ppm-lines [canvas]
  (concat (create-header-lines canvas)
          (format-lines canvas)))

(defn add-newlines [lines]
  (conj (vec (interpose \newline lines))
        \newline))

(defn canvas-to-ppm [canvas]
  (let [width (:width canvas)]
    (apply str (add-newlines (create-ppm-lines canvas)))))
