(ns raytracer.ray-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.point :as point]
            [raytracer.svector :as svector]
            [raytracer.matrix :as matrix]            
            [raytracer.ray :as ray]
            [raytracer.transform :as transform]
            [raytracer.materials :as materials]))

(deftest test-ray
  (testing "ray creation"
    (let [origin (point/point 1 2 3)
          direction (svector/svector 4 5 6)
          ray (ray/create origin direction)]
      (is (identical? origin (:origin ray)))
      (is (identical? direction (:direction ray))))))

(deftest test-position
  (let [ray (ray/create (point/point 2 3 4)
                        (svector/svector 1 0 0))]
    (testing "t=0"
      (is (v= (point/point 2 3 4)
              (ray/position ray 0))))
    (testing "t=1"
      (is (v= (point/point 3 3 4)
              (ray/position ray 1))))
    (testing "t=-1"
      (is (v= (point/point 1 3 4)
              (ray/position ray -1))))
    (testing "t=2.5"
      (is (v= (point/point 4.5 3 4)
              (ray/position ray 2.5))))))

(deftest test-sphere
  (let [sphere (ray/sphere)]
    (testing "create a sphere"
      (is (v= [0 0 0 1] (:center sphere)))
      (is (eps= 1.0 (:radius sphere)))
      (is (= (materials/material) (:material sphere)))
      (is (identical? matrix/identity-matrix (:transform sphere))))
    (testing "update a sphere's transformation"
      (let [transform (transform/translate 2 3 4)
            new-sphere (ray/change-transform sphere transform)]
        (is (identical? transform
                        (:transform new-sphere)))))
    (testing "a sphere may be assigned a material"
      (let [new-material (assoc (materials/material) :ambient 0.25)]
        (is (= new-material
               (:material (ray/change-material sphere new-material))))))))

(deftest test-same-sphere?
  (testing "returns the first argument if true, nil otherwise"
    (let [a (ray/change-transform (ray/sphere) (transform/scale 2 2 2))
          b (ray/change-transform (ray/sphere) (transform/scale 2 2 2))]
      (is (not (identical? a b)))
      (is (identical? a (ray/same-sphere? a b)))
      (is (nil? (ray/same-sphere? a (ray/sphere))))))
  (testing "two spheres are equal when they receive the same transform"
    (is (ray/same-sphere? (ray/change-transform (ray/sphere) (transform/scale 2 2 2))
                          (ray/change-transform (ray/sphere) (transform/scale 2 2 2)))))
  (testing "different transforms, different sphere"
    (is (not (ray/same-sphere? (ray/change-transform (ray/sphere) (transform/scale 2 2 3))
                               (ray/change-transform (ray/sphere) (transform/scale 2 2 2)))))))

(deftest test-sphere-intersection
  (let [sphere (ray/sphere)]
    (testing "a ray intersects a sphere at two points"
      (let [intersections (ray/intersect (ray/create (point/point 0 0 -5)
                                                     (svector/svector 0 0 1))
                                         sphere)]
        (is (= 2 (:count intersections)))
        (is (identical? sphere (:object (first (:values intersections)))))
        (is (identical? sphere (:object (second (:values intersections)))))
        (is (eps= 4 (:t (first (:values intersections)))))
        (is (eps= 6 (:t (second (:values intersections)))))))
    (testing "a ray intersects a sphere at a tangent"
      (let [intersections (ray/intersect (ray/create (point/point 0 1 -5)
                                                     (svector/svector 0 0 1))
                                         sphere)]
        (is (= 2 (:count intersections)))
        (is (identical? sphere (:object (first (:values intersections)))))
        (is (identical? sphere (:object (second (:values intersections)))))
        (is (eps= 5 (:t (first (:values intersections)))))
        (is (eps= 5 (:t (second (:values intersections)))))))
    (testing "a ray misses a sphere"
      (let [intersections (ray/intersect (ray/create (point/point 0 2 -5)
                                                     (svector/svector 0 0 1))
                                         sphere)]
        (is (= 0 (:count intersections)))))
    (testing "a ray originates inside a sphere"
      (let [intersections (ray/intersect (ray/create (point/point 0 0 0)
                                                     (svector/svector 0 0 1))
                                         sphere)]
        (is (= 2 (:count intersections)))
        (is (identical? sphere (:object (first (:values intersections)))))
        (is (identical? sphere (:object (second (:values intersections)))))
        (is (eps= -1 (:t (first (:values intersections)))))
        (is (eps= 1 (:t (second (:values intersections)))))))
    (testing "a sphere is behind a ray"
      (let [intersections (ray/intersect (ray/create (point/point 0 0 5)
                                                     (svector/svector 0 0 1))
                                         sphere)]
        (is (= 2 (:count intersections)))
        (is (identical? sphere (:object (first (:values intersections)))))
        (is (identical? sphere (:object (second (:values intersections)))))
        (is (eps= -6 (:t (first (:values intersections)))))
        (is (eps= -4 (:t (second (:values intersections)))))))
    (testing "intersecting a scaled sphere with a ray"
      (let [ray (ray/create (point/point 0 0 -5)
                            (svector/svector 0 0 1))
            sphere (ray/change-transform (ray/sphere)
                                         (transform/scale 2 2 2))
            intersections (ray/intersect ray sphere)]
        (is (= 2 (:count intersections)))
        (is (eps= 3 (:t (first (:values intersections)))))
        (is (eps= 7 (:t (second (:values intersections)))))))
    (testing "intersecting a translated sphere with a ray"
      (let [ray (ray/create (point/point 0 0 -5)
                            (svector/svector 0 0 1))
            sphere (ray/change-transform (ray/sphere)
                                         (transform/translate 5 0 0))
            intersections (ray/intersect ray sphere)]
        (is (= 0 (:count intersections)))))    
    ))

(deftest test-intersection
  (testing "create new intersection"
    (let [sphere (ray/sphere)
          intersection (ray/intersection 2.4 sphere)]
      (is (identical? sphere (:object intersection)))
      (is (= 2.4 (:t intersection))))))

(deftest test-intersections
  (testing "exciting \"intersections\" function"
    (let [i1 (ray/intersection 2.4 (ray/sphere))
          i2 (ray/intersection 2.8 (ray/sphere))
          intersections (ray/intersections i1 i2)]
      (is (= 2 (count intersections)))
      (is (identical? i1 (first intersections)))
      (is (identical? i2 (second intersections))))))


(defmacro hit-testcase [ & {:keys [message intersections-t expected]}]
  `(testing ~message
     (let [sphere# (ray/sphere)
           intersections# (apply ray/intersections (map #(ray/intersection % sphere#) ~intersections-t))
           hit# (ray/hit intersections#)]
       (is (or (and (not ~expected) (not hit#))
               (= (:t (nth intersections# ~expected)) (:t hit#)))))))

(deftest test-hit
  (hit-testcase :message "the hit, when all intersections have positive t"
                :intersections-t [1 2]
                :expected 0)
  (hit-testcase :message "the hit, when some intersections have negative t"
                :intersections-t [-1 1]
                :expected 1)
  (hit-testcase :message "the hit, when all intersections have negative t"
                :intersections-t [-2 -1]
                :expected nil)
  (hit-testcase :message "the hit is always the lowest nonnegative intersection"
                :intersections-t [5 7 -3 2 4]
                :expected 3))

(deftest test-transform
  (let [ray (ray/create (point/point 1 2 3)
                        (svector/svector 0 1 0))]
    (testing "translating a ray"
      (let [result (ray/transform ray (transform/translate 3 4 5))]
        (is (v= [4 6 8 1] (:origin result)))
        (is (v= [0 1 0 0] (:direction result)))))
    (testing "scaling a ray"
      (let [result (ray/transform ray (transform/scale 2 3 4))]
        (is (v= [2 6 12 1] (:origin result)))
        (is (v= [0 3 0 0] (:direction result)))))))

(deftest test-as-point
  (is (v= (point/point 1 2 3)
          (ray/as-point (svector/svector 1 2 3)))))

(deftest test-as-vector
  (is (v= (svector/svector 1 2 3)
          (ray/as-vector (point/point 1 2 3)))))

(deftest test-sphere-normal
  (let [sphere (ray/sphere)
        √2 (Math/sqrt 2)
        half√2 (/ √2 2)
        √3 (Math/sqrt 3)
        third√3 (/ √3 3)]
    (testing "The normal on a sphere at a point on the x axis"
      (is (v= (svector/svector 1 0 0)
              ((:normal sphere) (point/point 1 0 0)))))
    (testing "The normal on a sphere at a point on the y axis"
      (is (v= (svector/svector 0 1 0)
              ((:normal sphere) (point/point 0 1 0)))))
    (testing "The normal on a sphere at a point on the z axis"
      (is (v= (svector/svector 0 0 1)
              ((:normal sphere) (point/point 0 0 1)))))
    (testing "The normal on a sphere at a nonaxial point"
      (is (v= (svector/svector third√3 third√3 third√3)
              ((:normal sphere) (point/point third√3 third√3 third√3)))))
    (testing "Computing the normal on a translated sphere"
      (let [transformed-sphere (ray/change-transform sphere (transform/translate 0 1 0))]
        (is (v= (svector/svector 0 0.70711 -0.70711)
                ((:normal transformed-sphere) (point/point 0 1.70711 -0.70711))))))
    (testing "Computing the normal on a transformed sphere"
      (let [new-transform (transform/scale 1 0.5 1 (transform/rotate-z (/ Math/PI 5)))
            transformed-sphere (ray/change-transform sphere new-transform)]
        (is (v= (svector/svector 0 0.97014 -0.24254)
                ((:normal transformed-sphere) (point/point 0 half√2 (- half√2)))))))))

(deftest test-reflection
  (testing "Reflecting a vector approaching at 45°"
    (is (v= (svector/svector 1 1 0)
            (ray/reflect (svector/svector 1 -1 0)
                         (svector/svector 0 1 0)))))
  (testing "Reflecting a vector off a slanted surface"
    (let [√2 (Math/sqrt 2)
          half√2 (/ √2 2)]
      (is (v= (svector/svector 1 0 0)
              (ray/reflect (svector/svector 0 -1 0)
                           (svector/svector half√2 half√2 0)))))))
