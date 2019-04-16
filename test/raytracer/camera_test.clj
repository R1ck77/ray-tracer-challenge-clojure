(ns raytracer.camera-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.camera :as camera]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]
            [raytracer.transform :as transform]            
            [raytracer.canvas :as canvas]
            [raytracer.world :as world]))

(def halfπ (/ Math/PI 2))
(def partπ (/ Math/PI 4))
(def half√2 (/ (Math/sqrt 2) 2))

(deftest test-camera
  (testing "Constructing a camera"
    (let [h-size 160
          v-size 120
          fov halfπ]
      (is (= {:h-size h-size
              :v-size v-size
              :fov fov
              :transform matrix/identity-matrix
              :inverse-transform matrix/identity-matrix}
             (dissoc (camera/camera h-size v-size fov)
                     :pixel-size
                     :half-width
                     :half-height)))))
  (testing "The pixel size for a horizontal canvas"
    (is (eps= 0.01
              (:pixel-size (camera/camera 200 125 halfπ)))))
  (testing "The pixel size for a vertical canvas"
    (is (eps= 0.01
              (:pixel-size (camera/camera 125 200 halfπ))))))

(deftest test-set-transform
  (testing "The initial transform for a camera is the identity matrix"
    (is (v= matrix/identity-matrix (:transform (camera/camera 1 2 3)))))
  (testing "Change the transform matrix for a camera"
    (let [new-transform-matrix (transform/translate 1 2 3)]
      (is (= new-transform-matrix
             (:transform (camera/set-transform (camera/camera 1 2 3)
                                               new-transform-matrix))))))
  (testing "Changing the transform matrix also computes the inverse"
        (let [new-transform-matrix (transform/translate 1 2 3)]
      (is (= (matrix/invert new-transform-matrix 4)
             (:inverse-transform (camera/set-transform (camera/camera 1 2 3)
                                               new-transform-matrix)))))))

(deftest test-ray-for-pixel
  (testing "Constructing a ray through the center of the canvas"
    (let [camera (camera/camera 201 101 halfπ)
          ray (camera/ray-for-pixel camera 100 50)]
      (is (v= (point/point 0 0 0)
              (:origin ray)))
      (is (v= (svector/svector 0 0 -1)
              (:direction ray)))))
  (testing "Constructing a ray through a corner of the canvas"
    (let [camera (camera/camera 201 101 halfπ)
          ray (camera/ray-for-pixel camera 0 0)]
      (is (v= (point/point 0 0 0)
              (:origin ray)))
      (is (v= (svector/svector 0.66519, 0.33259, -0.66851)
              (:direction ray)))))
  (testing "Constructing a ray when the camera is transformed"
    (let [camera (camera/camera 201 101 halfπ)
          transform (transform/rotate-y partπ (transform/translate 0 -2 5)) 
          transformed-camera (camera/set-transform camera transform)
          ray (camera/ray-for-pixel transformed-camera 100 50)]
      (is (v= (point/point 0 2 -5)
              (:origin ray)))
      (is (v= (svector/svector half√2 0 (- half√2))
              (:direction ray))))))

(deftest test-render
  (testing "Rendering a world with a camera"
    (let [world (world/default-world)
          camera (camera/camera 11 11 halfπ)
          transform (world/view-transform (point/point 0 0 -5)
                                          (point/point 0 0 0)
                                          (svector/svector 0 1 0))]
      (v= [0.38066 0.47583 0.2855]
          (canvas/read (camera/render camera world) 5 5)))))

