(ns demo.projectile
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer [eps= eps4=]]
            [raytracer.svector :refer :all]
            [raytracer.point :refer :all]
            [raytracer.canvas :refer :all]))

(def dt 0.0001)

(defn- as-vector [[x y z]]
  (svector x y z))

(defn create-forces [& forces]
 (reduce add (svector 0 0 0) (map as-vector forces)))

(deftest test-create-forces
  (testing "no argument"
    (is (= (svector 0 0 0)
           (create-forces))))
  (testing "one argument"
    (is (= (svector 1 2 3)
           (create-forces [1 2 3]))))
  (testing "multiple arguments"
    (is (= (svector 5 5 5)
           (create-forces [1 2 3] [3 2 1] [1 1 1])))))

(defn create-object
  "Mass is 1"
  [position velocity]
  {:position (apply point position)
   :velocity (apply svector velocity)})

(deftest test-create-object
  (testing "creating an object"
    (is (= {:position [1 2 3 1], :velocity [3 2 1 0]}
           (create-object [1 2 3] [3 2 1])))))

(defn tick
  ([object forces]
   (tick object forces dt))
  ([{p :position, v :velocity} forces dt]
   {:position (add p (mul v dt))
    :velocity (add v (mul forces dt))}))

(defn- eq-obj [a b]
  (and (eps4= (:position a) (:position b))
       (eps4= (:velocity a) (:velocity b))))

(deftest test-eq-obj
  (testing "similar object"
    (is (eq-obj {:position (point 1 2 3), :velocity (svector 4 5 6)}
                {:position (point 1 2 3), :velocity (svector 4 5 6)})))
  (testing "different position"
    (is (not (eq-obj {:position (point 1 2 3), :velocity (svector 4 5 6)}
                     {:position (point 1 2 3.1), :velocity (svector 4 5 6)}))))
  (testing "different velocity"
    (is (not (eq-obj {:position (point 1 2 3), :velocity (svector 4 5 6)}
                     {:position (point 1 2 3), :velocity (svector 4 5.1 6)})))))

(deftest test-tick
  (testing "no velocity, no forces"
    (let [object (create-object [3 1 4] [0 0 0])]
      (is (eq-obj object
                  (tick object (svector 0 0 0) 0.1)))))
  (testing "velocity, no forces"
    (is (eq-obj (create-object [3.4 1.3 4.2] [4 3 2])
                (tick (create-object [3 1 4] [4 3 2])
                      (svector 0 0 0) 0.1))))
  (testing "no velocity, forces"
    (is (eq-obj (create-object [3 1 4] [0.4 0.2 0.3])
                (tick (create-object [3 1 4] [0 0 0])
                      (svector 4 2 3) 0.1))))
  (testing "general case"
    (is (eq-obj (create-object [3.1 1.2 4.3] [1.4 2.2 3.3])
                (tick (create-object [3 1 4] [1 2 3])
                      (svector 4 2 3) 0.1)))))

(defn- text-writer
  ([])
  ([x y]
   (println x y)))

(defn- create-scaler-f
  [point-min point-max canvas-size]
  (let [convert-f (fn [point point-min point-max canvas-max]
                    (int (* canvas-max (/ (- point point-min) (- point-max point-min)))))]
    (fn [x y]
      (map convert-f [x y] point-min point-max canvas-size))))

(defn- reduce-data [data f]
  (reduce #(map f %2 %) data))

(defn- write-to-canvas
  [canvas data]
  (let [canvas-height (:height canvas)
        scaler-f (create-scaler-f (reduce-data data min)
                                  (reduce-data data max)
                                  (list (dec (:width canvas))
                                        (dec canvas-height)))]
    (reduce (fn [canvas [x y]]
              (let [[px py] (scaler-f x y)]
                (try
                  (write canvas px (- canvas-height py 1) [1 0 0])      
                  (catch Exception e canvas))))
                  canvas data)))

(defn save-to-file [width height data]
  (let [ppm-content (canvas-to-ppm
                     (write-to-canvas (create-canvas width height) data))]
    (spit "output.ppm" ppm-content)))

(defn create-canvas-writer [width height]
  (let [data (atom [])]
    (fn
      ([x y]
       (swap! data #(conj % (list x y))))
      ([]
       (save-to-file width height @data)))))

(defn- simulate
  ([projectile force dt]
   (simulate projectile force dt (create-canvas-writer 320 200)))
  ([projectile force dt writer]
   (let [result (tick projectile force dt)
         position (:position result)]
     (writer (get position 0) (get position 1))
     (if (> (second position) 0)
       (recur result force dt writer)
       (writer)))))

(defn- create-projectile [angle-rad muzzle-speed]
  (create-object [0 1 0]
                 [(* muzzle-speed (Math/cos angle-rad))
                  (* muzzle-speed (Math/sin angle-rad))
                  0]))

(defn deg-to-rad [angle-deg]
  (* Math/PI (/ angle-deg 180)))

(deftest testing-deg-to-rad
  (testing "check 45° conversion"
   (is (eps= (/ Math/PI 4) (deg-to-rad 45)))))

(defn simulate-dragless-cannon
  [angle-deg muzzle-speed]
  (simulate (create-projectile (deg-to-rad angle-deg) muzzle-speed)
            (create-forces [0 -9.81 0]) 0.1))
