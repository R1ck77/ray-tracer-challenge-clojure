(ns demo.timed-run
  (:require [demo.reflection-demo :as demo]
            [raytracer.world :as world]
            [raytracer.camera :as camera]))

(defn -main [& args]
  (with-redefs [demo/*image-resolution* [640 480]
                camera/*map* pmap
                world/*maximum-reflections* 5]
    (time (demo/render-demo))
    (shutdown-agents)))
