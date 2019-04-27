;;; 2D Perlin noise
(ns raytracer.perlin
  (:require [raytracer.svector :as svector]))

(def neighbors [[0 0] [1 0] [0 1] [1 1]])

(defn- empty-grid [width height]
  (into-array (take height (repeatedly #(make-array java.util.List width)))))

(defn- random-simm []
  (- (rand 2) 1))

(defn- random-gradient []
  (svector/normalize (svector/svector (random-simm) (random-simm) 0)))

(defn- fill-grid [grid rows columns]
  (doseq [i (range rows)
          j (range columns)]
    (aset grid i j (random-gradient))))

(defn create-grid [columns rows]
  (doto (empty-grid columns rows)
    (fill-grid rows columns)))

(defn create-perlin-data [x-scale y-scale]
  {:x-scale x-scale
   :y-scale y-scale
   :grid (create-grid x-scale y-scale)})

(defn scale-point
  "Scale the point so that it fits inside the proper grid after periodic boundary translation"
  [perlin-data [x y]]
  (vector (* x (:x-scale perlin-data))
          (* y (:y-scale perlin-data))))

(defn fade [t]
  (* (* t t t)
     (+ (* t (- (* 6 t) 15))
        10)))

(defn- lerp [a0 a1 w]
  (+ a0 (* (fade w) (- a1 a0))))

(defn- compute-corners [x y]
  (vec
   (map (fn from-delta [[dx dy]]
          (vector (+ (int (Math/floor x)) dx)
                  (+ (int (Math/floor y)) dy)))
        neighbors)))

(defn get-scaled-point-bounds [scaled-point]
  {:corners (apply compute-corners scaled-point)
   :point scaled-point})

(defn get-pbc [perlin-data [x y :as point]]
  (aget (:grid perlin-data)
        (mod y (:y-scale perlin-data))
        (mod x (:x-scale perlin-data))))

(defn- recover-gradients
  [perlin-data corners]
  (map #(get-pbc perlin-data %) corners))

(defn- compute-dot-products [gradients distances]
  (vec
   (map (fn [[gx gy] [dx dy]]
          (+ (* gx dx) (* gy dy))) gradients distances)))

(defn- compute-distances [corners [x y]]
  (map (fn corner-point-distance [[cx cy]]
         (vector (- cx x) (- cy y)))
       corners))

(defn assoc-dot-products [perlin-data {:keys [corners point] :as temp-data}]
  (let  [[x y] point]
    (assoc temp-data
           :dots (compute-dot-products (recover-gradients perlin-data corners)
                                       (compute-distances corners point)))))

(defn interpolate [{:keys [corners point dots]}]
  (let [[px py] point
        b (lerp (nth dots 2) (nth dots 3) (- (first point) (int (first point))))
        a (lerp (nth dots 0) (nth dots 1) (- (first point) (int (first point))))
        c (lerp a b (- (second point) (int (second point) )))]
    c
    ))

(defn noise [perlin-data point]
  (interpolate
   (assoc-dot-products perlin-data
                       (get-scaled-point-bounds (scale-point perlin-data point)))))
