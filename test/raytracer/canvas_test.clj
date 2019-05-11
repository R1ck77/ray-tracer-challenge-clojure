(ns raytracer.canvas-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [raytracer.color :as color]
            [raytracer.test-utils :refer :all]
            [raytracer.canvas :as canvas]))

(def red (color/color 1 0 0))
(def black (color/color 0 0 0))

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
                      #(c= black
                           (canvas/read canvas % %2))))))

(deftest test-write-pixel
  (testing "pixel are written as expected"
    (let [canvas (canvas/create-canvas 10 20)
          updated-canvas (canvas/write canvas 2 3 red)]
      (for-each-pixel 10 20
                      (fn [x y]
                        (c= (if (= [2 3] [x y])
                              red
                              black)
                            (canvas/read updated-canvas x y)))))))

(deftest test-fix-length
  (testing "doesn't change the size of lines that are short already"
    (is (= ["a b c d e f"] (canvas/fix-length (list "a b c d e f") 11)))
    (is (= ["a b c d e f"] (canvas/fix-length (list "a b c d e f") 12))))
  (testing "splits line that's too long"
    (is (= ["a b c d" "e f"] (canvas/fix-length (list "a b c d e f") 7)))
    (is (= ["a b c d" "e f"] (canvas/fix-length (list "a b c d e f") 8))))
  (testing "lines can be split multiple times"
    (is (= ["a b" "c d" "e f" "g"] (canvas/fix-length (list "a b c d e f g") 3)))
    (is (= ["a b" "c d" "e f" "g"] (canvas/fix-length (list "a b c d e f g") 4)))))

(deftest test-canvas-to-ppm
  (testing "the ppm header is created correctly"
    (let [canvas (canvas/create-canvas 5 3)
          ppm-text (canvas/canvas-to-ppm canvas)]
      (is (= ["P3" "5 3" "255"]
             (take 3 (string/split-lines ppm-text))))))
  (testing "the pixels are written correctly"
    (let [canvas (canvas/create-canvas 5 3)
          c1 (color/color 1.5 0 0)
          c2 (color/color 0 0.5 0)
          c3 (color/color -0.5 0 1)
          ppm (canvas/canvas-to-ppm (-> canvas
                                        (canvas/write 0 0 c1)
                                        (canvas/write 2 1 c2)
                                        (canvas/write 4 2 c3)))]
      (is (= ["255 0 0 0 0 0 0 0 0 0 0 0 0 0 0"
              "0 0 0 0 0 0 0 128 0 0 0 0 0 0 0"
              "0 0 0 0 0 0 0 0 0 0 0 0 0 0 255"]
             (take 3 (drop 3 (string/split-lines ppm)))))))
  (testing "lines don't exceed 70 characters"
    (let [color (color/color 1 0.8 0.6)
          lines (string/split-lines (canvas/canvas-to-ppm (reduce (fn [canvas [x y]]
                                                                    (canvas/write canvas x y color))
                                                                  (canvas/create-canvas 10 2)
                                                                  (for [j (range 2) i (range 10)] (vector i j)))))]
      (is (= ["255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204"
              "153 255 204 153 255 204 153 255 204 153 255 204 153"
              "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204"
              "153 255 204 153 255 204 153 255 204 153 255 204 153"]
             (take 4 (drop 3 lines))))))
  (testing "converted canvas ends with newline"
    (let [canvas (canvas/create-canvas 5 3)
          ppm-text (canvas/canvas-to-ppm canvas)]
      (is (= \newline (last ppm-text))))))

