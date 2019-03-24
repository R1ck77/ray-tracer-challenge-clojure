(ns raytracer.canvas-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [raytracer.tuples :refer [eps3=]]
            [raytracer.canvas :as canvas]))

(def red [1 0 0])
(def black [0 0 0])

(defmacro for-each-pixel
  [width height function]
  `(do
     ~@(for [j (range height)
            i (range width)]
        `(is (~function ~i ~j)))))

(deftest test-create-canvas
  (testing "canvas size"
    (let [canvas (canvas/create-canvas 10 20)]
      (is (= 10 (:width canvas)))
      (is (= 20 (:height canvas)))))
  (testing "canvas starting color is black"
    (let [canvas (canvas/create-canvas 10 20)]
      (for-each-pixel 10 20
                      #(eps3= black
                              (canvas/read canvas % %2))))))

(deftest test-write-pixel
  (testing "pixel are written as expected"
    (let [canvas (canvas/create-canvas 10 20)
          updated-canvas (canvas/write canvas 2 3 red)]
      (for-each-pixel 10 20
                      (fn [x y]
                        (= (if (= [2 3] [x y])
                             red
                             black)
                           (canvas/read updated-canvas x y)))))))

(deftest test-canvas-to-ppm
  (testing "the ppm header is created correctly"
    (let [canvas (canvas/create-canvas 5 3)
          ppm-text (canvas/canvas-to-ppm canvas)]
      (is (= ["P3" "5 3" "255"]
             (take 3 (string/split-lines ppm-text))))))
  (testing "the pixels are written correctly"
    (let [canvas (canvas/create-canvas 5 3)
          c1 [1.5 0 0]
          c2 [0 0.5 0]
          c3 [-0.5 0 1]
          ppm (canvas/canvas-to-ppm (-> canvas
                                        (canvas/write 0 0 c1)
                                        (canvas/write 2 1 c2)
                                        (canvas/write 4 2 c3)))]
      (println ppm)
      (is (= ["255 0 0 0 0 0 0 0 0 0 0 0 0 0 0"
              "0 0 0 0 0 0 0 128 0 0 0 0 0 0 0"
              "0 0 0 0 0 0 0 0 0 0 0 0 0 0 255"]
             (take 3 (drop 3 (string/split-lines ppm))))))))
