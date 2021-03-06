(ns raytracer.shapes.cone-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.const :as const]
            [raytracer.ray :as ray]
            [raytracer.tuple :as tuple]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.transform :as transform]
            [raytracer.shapes :as shapes]
            [raytracer.shapes.cone :as cone]
            [raytracer.shapes.shared :as shared]
            [raytracer.shapes.bounding-box :as bounding-box]))

(def a-cone (cone/cone))

(def √2 (Math/sqrt 2))

(deftest test-bounding-box-protocol
  (testing "Bounding box for an infinite cone"
    (is (= [(point/point -4.0 -4.0 -4.0)
            (point/point 4.0 4.0 4.0)]
           (bounding-box/get-corners (cone/cone :closed false, :minimum -4, :maximum 4)))))
  (testing "Bounding box for a cone between -4, 4"
    (is (= [(point/point -4.0 -4.0 -4.0)
            (point/point 4.0 4.0 4.0)]
           (bounding-box/get-corners (cone/cone :closed true, :minimum -4, :maximum 4)))))
  (testing "Bounding box for a cone between -10, -3"
    (is (= [(point/point -10.0 -10.0 -10.0)
            (point/point 10.0 -3.0 10.0)]
           (bounding-box/get-corners (cone/cone :closed true, :minimum -10, :maximum -3)))))
  (testing "Bounding box for a cone between 3, 100"
    (is (= [(point/point -100.0 3.0 -100.0)
            (point/point 100.0 100.0 100.0)]
           (bounding-box/get-corners (cone/cone :closed true, :minimum 3, :maximum 100)))))
    (testing "Bounding box for a cone between -50, -50 (degenerate)"
    (is (= [(point/point -50.0 -50.0 -50.0)
            (point/point 50.0 -50.0 50.0)]
           (bounding-box/get-corners (cone/cone :closed true, :minimum -50, :maximum -50))))))

(deftest test-equality
  (testing "Two cones are equals if the share the same characteristics"
    (is (= (cone/cone)
           (cone/cone)))
    (let [transformed-cone (shared/change-transform a-cone (transform/translate 1 2 3 ))]
      (is (= transformed-cone transformed-cone))
      (is (not= transformed-cone (cone/cone))))))

(deftest test-constructor
  (testing "The default minimum and maximum for a cone"
    (is (= const/inf (:maximum a-cone)))
    (is (= const/neg-inf (:minimum a-cone))))
  (testing "A cone can be created with a set minimum"
    (let [cone (cone/cone :minimum 12)]
      (is (= const/inf (:maximum cone)))
      (is (= 12 (:minimum cone)))))
  (testing "A cone can be created with a set maximum"
    (let [cone (cone/cone :maximum 100)]
      (is (= 100 (:maximum cone)))
      (is (= const/neg-inf (:minimum a-cone)))))
  (testing "A cone can be created with a set minimum and maximum"
    (let [cone (cone/cone :maximum 100 :minimum 12)]
      (is (= 12 (:minimum cone)))
      (is (= 100 (:maximum cone)))))
  (testing "The default closed value for a cone"
    (is (not (:closed a-cone))))
  (testing "Filled cones can be created"
    (is (:closed (cone/cone :closed true)))))

(defmacro test-compute-cone-normal [point expected-normal]
  `(is
    (t= (tuple/normalize (apply svector/svector ~expected-normal))
        (shared/compute-normal a-cone (apply point/point ~point)))))

(defmacro test-intersecting-cone-with-end-caps [origin direction expected-count]
  `(let [cone# (cone/cone :closed true
                          :minimum -0.5
                          :maximum 0.5)
         ray# (ray/ray (apply point/point ~origin)
                       (apply svector/svector ~direction))]
     (is (= ~expected-count
            (count (shared/local-intersect cone# ray#))))))

(defmacro test-intersecting-cone-with-ray [origin direction expected-result]
  `(let [cone# (cone/cone)
         ray# (ray/ray (apply point/point ~origin)
                       (apply svector/svector ~direction))]
     (is (v= ~expected-result
             (map :t (shared/local-intersect cone# ray#))))))

(deftest test-local-intersect
  (testing "Intersecting a cone with a ray"
    (test-intersecting-cone-with-ray [0 0 -5] [0 0 1] [5 5])
    (test-intersecting-cone-with-ray [0 0 -5] [1 1 1] [8.66025 8.66025])
    (test-intersecting-cone-with-ray [1 1 -5] [-0.5 -1 1] [4.55006 49.44994]) )

  (testing "Intersecting a cone's end caps"
    (test-intersecting-cone-with-end-caps [0, 0, -5]    [0, 1, 0] 0)
    (test-intersecting-cone-with-end-caps [0, 0, -0.25] [0, 1, 1] 2)
    (test-intersecting-cone-with-end-caps [0, 0, -0.25] [0, 1, 0] 4)))

(deftest test-local-intersect
    (testing "Intersecting a cone with a ray parallel to one of its halves"
    (is [0.35355]
        (vec (map :t (shared/local-intersect a-cone
                                             (ray/ray (point/point 0 0 -1)
                                                      (tuple/normalize (svector/svector 0 1 1)))))))))

(deftest test-compute-normal
  (testing "Computing the normal vector on a cone"
    (test-compute-cone-normal [0 0 0] [0 0 0])
    (test-compute-cone-normal [1 1 1] [1 (- (Math/sqrt 2)) 1])
    (test-compute-cone-normal [-1 -1 0] [-1 1 0])))

