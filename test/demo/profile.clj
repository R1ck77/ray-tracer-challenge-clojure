(ns demo.profile
  (:require [demo.cubes-demo :as demo]
            [raytracer.world :as world]
            [raytracer.camera :as camera]))

(defn -main [& args]
  (with-redefs [demo/*image-resolution* [(* 2 1920) (* 2 1080)]
                camera/*map* map
                world/*maximum-reflections* 1]
    (time (demo/render-demo))))
