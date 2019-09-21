(ns raytracer.shapes.bounding-box-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :as tu]
            [raytracer.const :as const]
            [raytracer.point :as point]
            [raytracer.tuple :as tuple]
            [raytracer.svector :as svector]
            [raytracer.ray :as ray]
            [raytracer.transform :as transform]
            [raytracer.shapes.bounding-box :as bounding-box]))

(def small-value (/ const/EPSILON 100))

(def an-invisible-box (bounding-box/->InvisibleBox))
(def an-infinite-box (bounding-box/->InfiniteBox))
(def a-bounding-box (bounding-box/->DefaultBox (point/point -3 -1 -7)
                                               (point/point 10 17 13)))

(deftest test-invisible-box
  (testing "Invisible boxes are trasformed to themselves"
    (is (identical? an-invisible-box
                    (bounding-box/transform an-invisible-box
                                            (transform/scale 1000 1000 1000)))))
  (testing "Merging with an invisible box always returns the other box"
    (is (identical? an-invisible-box
                    (bounding-box/merge (bounding-box/->InvisibleBox)
                                            an-invisible-box)))
    (is (identical? an-infinite-box
                    (bounding-box/merge an-invisible-box
                                            an-infinite-box)))
    (is (identical? a-bounding-box
                (bounding-box/merge an-invisible-box
                                    a-bounding-box))))
  (testing "Invisible boxes are invisible and not infinite"
    (is (bounding-box/invisible? an-invisible-box))
    (is (not (bounding-box/infinite? an-invisible-box))))
  (testing "You can't get the extremes of an invisible box"
    (is (thrown? UnsupportedOperationException (bounding-box/get-extremes an-invisible-box))))
  (testing "You can't hit an invisible box, no matter how hard you try"
    (is (not (bounding-box/hit an-invisible-box (ray/ray (point/point 0 0 0)
                                                         (svector/svector 1 0 0)))))))

(deftest test-infinite-box
  (testing "Infinite boxes are trasformed to themselves"
    (is (identical? an-infinite-box
                    (bounding-box/transform an-infinite-box
                                            (transform/scale 1000 1000 1000)))))
  (testing "Merging with an infinite box always returns itself"
    (is (identical? an-infinite-box
                    (bounding-box/merge an-infinite-box
                                        (bounding-box/->InfiniteBox))))
    (is (identical? an-infinite-box
                    (bounding-box/merge an-infinite-box
                                        an-invisible-box)))
    (is (identical? an-infinite-box
                    (bounding-box/merge an-infinite-box
                                        a-bounding-box))))
  (testing "Infinite boxes are infinite and not invisible"
    (is (bounding-box/infinite? an-infinite-box))
    (is (not (bounding-box/invisible? an-infinite-box))))
  (testing "You can't get the extremes of an infinite box"
    (is (thrown? UnsupportedOperationException (bounding-box/get-extremes an-infinite-box))))
  (testing "You can't avoid an invisible box, no matter how hard you try"
    (is (bounding-box/hit an-infinite-box (ray/ray (point/point 0 0 0)
                                                   (svector/svector 1 0 0))))))

(deftest test-default-box
  (testing "Returns the correct extremes"
    (let [min (point/point 1 2 3)
          max (point/point 4 5 6)]      
     (is (= {:min min, :max max}
            (bounding-box/get-extremes (bounding-box/->DefaultBox min max))))))
  (testing "Merging a bounding box with an infinite one"
    (is (identical? an-infinite-box
                    (bounding-box/merge a-bounding-box an-infinite-box))))
  (testing "Merging a bounding box with an invisible one"
    (is (identical? a-bounding-box
                    (bounding-box/merge a-bounding-box an-invisible-box))))
  (testing "Merging a bounding box with another"
    (is (= (bounding-box/->DefaultBox (point/point -14 -23 -100)
                                      (point/point 2000 1000 300))
           (bounding-box/merge (bounding-box/->DefaultBox (point/point -10 12 -100)
                                                          (point/point 2000 1000 7))
                               (bounding-box/->DefaultBox (point/point -14 -23 -7)
                                                          (point/point 100 200 300))))))
  (testing "Common boxes can be hit or missed"
    (is (bounding-box/hit (bounding-box/->DefaultBox (point/point 1 1 1)
                                                     (point/point 2 2 2))
                          (ray/ray (point/point 0 1.0001 1.0001)
                                   (svector/svector 1 0 0))))
    (is (not
         (bounding-box/hit (bounding-box/->DefaultBox (point/point 1 1 1)
                                                      (point/point 2 2 2))
                           (ray/ray (point/point 0 0 0)
                                    (svector/svector 1 0 0))))))
  (testing "Transforming the extremes of a common box"
    (is (tu/bb= (bounding-box/->DefaultBox (point/point (- const/√2) -1.0 (- const/√2))
                                      (point/point const/√2 1.0 const/√2))
           (bounding-box/transform (bounding-box/->DefaultBox (point/point -1 -1 -1)
                                                              (point/point 1 1 1))
                                   (transform/rotate-y const/quarter𝛑))))))

(deftest test-create-box
  (testing "Creating a standard box"
    (let [min (point/point -10 -2 -40)
          max (point/point 12 23 54)]
      (is (= (bounding-box/->DefaultBox min max)
             (bounding-box/create-box min max)))))
  (testing "Creating a standard box from small coordinates"
    (let [min (point/point -1e-250 -1e-250 -1e-250)
          max (point/point 1e-250 1e-250 1)]
      (is (= (bounding-box/->DefaultBox min max)
             (bounding-box/create-box min max)))))  
  (testing "Creating an infinite box"
    (let [min (point/point -10 -2 -1e250)
          max (point/point 12 23 54)]
      (is (= an-infinite-box
             (bounding-box/create-box min max)))))
  (testing "Creating an invisible box"
    (let [min (point/point (- 100 1e-200) (- 2 1e-200) 3)
          max (point/point (+ 100 1e-200) (+ 2 1e-200) 3)]
      (is (= an-invisible-box
             (bounding-box/create-box min max)))))
  (testing "Being infinite takes over being infinitesimal"
        (let [min (point/point (- 100 1e-200) 4 (- 12 1e201))
              max (point/point (+ 100 1e-200) (+ 4 1e-300) (+ 12 1e200))]
      (is (= an-infinite-box
             (bounding-box/create-box min max))))))
