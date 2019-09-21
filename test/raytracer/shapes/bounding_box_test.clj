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
                                    (svector/svector 1 0 0)))))))

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


(comment (deftest test-extremes-for-points
   (testing "Returns the unmodified point if there is only one"
     (is (= [(point/point 1 2 3) (point/point 1 2 3)]
            (bounding-box/extremes-from-points #{(point/point 1 2 3)}))))
   (testing "Returns the minimum and maximum values for all coordinates, respectively"
     (is (= [(point/point -1000 -100 1) (point/point 5 1000 3)]
            (bounding-box/extremes-from-points #{(point/point 1 2 3)
                                                 (point/point -1000 1000 2)
                                                 (point/point 5 -100 1)})))))

 (deftest test-box-points-from-extremes
   (testing "Convert 2 points in a bounding box to a list of box vertices"
     (is (= #{(point/point 0, 0, 0)
              (point/point 10 0 0)
              (point/point 0 10 0)
              (point/point 10 10 0)
              (point/point 0 0 10)
              (point/point 10 0 10)
              (point/point 0 10 10)
              (point/point 10 10 10)}
            (bounding-box/box-points-from-extremes (point/point 0 0 0)
                                                   (point/point 10 10 10))))))

 (deftest test-almost-identical
   (testing "Returns true for points where each coordinate is different by less than const/EPSILON"
     (is (bounding-box/almost-identical (tuple/tuple (+ 1 small-value) (+ 2 small-value) 3 4) (tuple/tuple (- 1 small-value) 2 (+ 3 small-value) (+ 4 small-value))))
     (is (bounding-box/almost-identical (tuple/tuple 1.0 2.0 3.0 4.0) (tuple/tuple 1 2 3 4))))
   (testing "Returns false if any coordinate differs for EPSILON or more"
     (let [delta (* const/EPSILON 1.1)]
       (is (not (bounding-box/almost-identical (tuple/tuple delta 2 3 12) (tuple/tuple 0 2 3 12))))
       (is (not (bounding-box/almost-identical (tuple/tuple 0 (+ 2 delta) 3 12) (tuple/tuple 0 2 3 12))))
       (is (not (bounding-box/almost-identical (tuple/tuple 0 2 (+ 3 delta) 12) (tuple/tuple 0 2 3 12))))
       (is (not (bounding-box/almost-identical (tuple/tuple 0 2 3 (+ 12 delta)) (tuple/tuple 0 2 3 12))))
       (is (not (bounding-box/almost-identical (tuple/tuple 0 2 3 12) (tuple/tuple delta 2 3 12))))
       (is (not (bounding-box/almost-identical (tuple/tuple 0 2 3 12) (tuple/tuple 0 (+ 2 delta) 3 12))))
       (is (not (bounding-box/almost-identical (tuple/tuple 0 2 3 12) (tuple/tuple 0 2 (+ 3 delta) 12))))
       (is (not (bounding-box/almost-identical (tuple/tuple 0 2 3 12) (tuple/tuple 0 2 3 (+ 12 delta))))))))

 (defn- same-extremes [[point-a-min point-a-max] [point-b-min point-b-max]]
   (and (bounding-box/almost-identical point-a-min point-b-min)
        (bounding-box/almost-identical point-b-max point-b-max)))

 (deftest test-compute-filtered-transformed-extremes
   (testing "Returns an empty collection if the input points are almost identical"
     (let [extremes [(point/point small-value (- small-value) small-value)
                     (point/point 0 0.0 0.0)]
           transform (transform/translate 10 20 30 (transform/rotate-x const/half𝛑))]
       (is (empty? (bounding-box/compute-filtered-transformed-extremes extremes transform)))))
   (testing "Returns the same points (within error) if the transform is the identity matrix"
     (let [extremes [(point/point 1 2 3)
                     (point/point 5 6 7)]]
       (is (= '(true true)
              (map bounding-box/almost-identical
                   extremes
                   (bounding-box/compute-filtered-transformed-extremes extremes (transform/rotate-x (- const/half𝛑) (transform/rotate-x const/half𝛑))))))))
   (testing "Returns a properly rotated cube if you turn the cube by 45 degrees"
     (let [extremes [(point/point -2 -2 -2)
                     (point/point 2 2 2)]
           diagonal (* 2 (Math/sqrt 2))
           expected [(point/point (- diagonal) -2 (- diagonal))
                     (point/point diagonal 2 diagonal)]]
      
       (is (same-extremes expected
                          (bounding-box/compute-filtered-transformed-extremes extremes
                                                                              (transform/rotate-y const/quarter𝛑))))))
   (testing "Returns a an empty collection if the input points are collapsed to a single point"
     (let [extremes [(point/point -10 -10 -10)
                     (point/point 3 3 3)]]      
       (is (empty? (bounding-box/compute-filtered-transformed-extremes extremes
                                                                       (transform/scale 0 0 0)))))))

 (deftest test-transform-extremes
   (testing "Accounts for transformed vertices"
     (let [[actual-min actual-max] (bounding-box/transform-extremes [(point/point -1 -1 -1)
                                                                     (point/point 1 1 1)]
                                                                    (transform/rotate-y const/quarter𝛑))]
       (is (tu/t= actual-min (point/point (- const/√2) -1 (- const/√2))))
       (is (tu/t= actual-max (point/point const/√2 1 const/√2))))))
 )
