;;; 2D Perlin noise
(ns raytracer.perlin)

(defn- empty-grid [dimensions]
  (apply (partial make-array java.util.List) (reverse dimensions)))

(defn- random-simm []
  (- (rand 2) 1))

(defn- random-gradient [dimensions]
  (let [v  (take dimensions (repeatedly random-simm))
        size (Math/sqrt (apply + (map #(* % %) v)))]
    (vec (if (> size 1e-6)
           (map #(/ % size) v)
           (random-gradient dimensions)))))

(defn- recursive-fill-array
  [f supplier dimensions]
  (if (not (empty? dimensions))
    (let [n (first dimensions)]
      (doseq [i (range n)]
        (recursive-fill-array (partial f i) supplier (rest dimensions))))
    (f (supplier))))

(defn fill-array
  "Fill the array with the specific value.

  Dimensions is in the user expected order, supplier generates value to set."
  [array supplier dimensions]
  (recursive-fill-array (partial aset array)
                        supplier
                        (reverse dimensions)))


(defn- fill-grid [grid dimensions]
  (fill-array grid #(random-gradient (count dimensions)) dimensions))

(defn create-grid [dimensions]
  (doto (empty-grid dimensions)
    (fill-grid dimensions)))

(defn create-perlin-data [dimensions]
  {:dimensions dimensions
   :neighbors [[0 0] [1 0] [0 1] [1 1]]
   :grid (create-grid dimensions)})

(defn scale-point
  "Scale the point so that it fits inside the proper grid after periodic boundary translation"
  [perlin-data point]
  (vec (map * (:dimensions perlin-data) point)))

(defn fade [t]
  (* (* t t t)
     (+ (* t (- (* 6 t) 15))
        10)))

(defn- lerp [a0 a1 w]
  (+ a0 (* (fade w) (- a1 a0))))

(defn- compute-corners [neighbors x y]
  (vec
   (map (fn from-delta [[dx dy]]
          (vector (+ (int (Math/floor x)) dx)
                  (+ (int (Math/floor y)) dy)))
        neighbors)))

(defn get-scaled-point-bounds [neighbors scaled-point]
  {:corners (apply compute-corners neighbors scaled-point)
   :point scaled-point})

(defn get-pbc [perlin-data [x y :as point]]
  (apply (partial aget (:grid perlin-data))
         ;;; TODO/FIXME name this thing!
         (reverse (map #(mod % %2) point (:dimensions perlin-data)))))

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
        b (lerp (nth dots 2) (nth dots 3) (- (first point) (Math/floor (first point))))
        a (lerp (nth dots 0) (nth dots 1) (- (first point) (Math/floor (first point))))
        c (lerp a b (- (second point) (Math/floor (second point) )))]
    c
    ))

(def half√2 (/ (Math/sqrt 2) 2))

(defn noise [perlin-data point]
  (/ (interpolate
    (assoc-dot-products perlin-data
                        (get-scaled-point-bounds (:neighbors perlin-data)
                                                 (scale-point perlin-data point))))
     half√2))
