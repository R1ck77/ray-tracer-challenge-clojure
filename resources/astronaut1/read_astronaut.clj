;;; Quick paste of a REPL session to read the file
(require '[clojure.java.io :as io])
(def is (java.util.zip.GZIPInputStream. (io/input-stream "astronaut1.obj.gz")))
(clojure.string/split-lines  (slurp is))
