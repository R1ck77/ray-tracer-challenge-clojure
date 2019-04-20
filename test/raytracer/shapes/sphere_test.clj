(ns raytracer.shape.sphere-test
  (:require [clojure.test :refer :all]
            [raytracer.test-utils :refer :all]
            [raytracer.svector :as svector]
            [raytracer.shapes :as shapes]
            [raytracer.point :as point]
            [raytracer.transform :as transform]
            [raytracer.shapes.sphere :as sphere]))

(deftest test-sphere-normal
  (let [sphere (sphere/sphere)
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
      (let [transformed-sphere (shapes/change-transform sphere (transform/translate 0 1 0))]
        (is (v= (svector/svector 0 0.70711 -0.70711)
                ((:normal transformed-sphere) (point/point 0 1.70711 -0.70711))))))
    (testing "Computing the normal on a transformed sphere"
      (let [new-transform (transform/scale 1 0.5 1 (transform/rotate-z (/ Math/PI 5)))
            transformed-sphere (shapes/change-transform sphere new-transform)]
        (is (v= (svector/svector 0 0.97014 -0.24254)
                ((:normal transformed-sphere) (point/point 0 half√2 (- half√2)))))))))
