(ns raytracer.shapes.obj
  (:require [raytracer.wavefront.parser :as parser]
            [raytracer.shapes.group :as group]
            [raytracer.shapes.optimizers.bisection-recursive :as bisection-recursive]))

(def ^:dynamic *max-objects-per-group* 10)

(defn obj [wavefront-location & {:as args-map}]
  (let [result (parser/parse (slurp wavefront-location))]
    (when (and (:log-skipped args-map) (not (empty? (:ignored result))))
      (println (format "%d lines skipped during parsing…" (count (:ignored result)))))
    (group/optimize (group/group
                     (map group/group (parser/get-non-empty-groups result)))
                    (bisection-recursive/create *max-objects-per-group*))))
