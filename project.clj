(defproject raytracer "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.9.0"]]
  :main ^:skip-aot raytracer.core
  :target-path "target/%s"
  :java-source-paths ["src-java"]
  :profiles {:demo-coverage {:main ^:skip-aot demo.all-demos-coverage}
             :uberjar {:aot :all
                       :global-vars {*warn-on-reflection* true}}             
             :visual-vm {
                         :jvm-opts ["-Dcom.sun.management.jmxremote"
                                    "-Dcom.sun.management.jmxremote.ssl=false"
                                    "-Dcom.sun.management.jmxremote.authenticate=false"
                                    "-Dcom.sun.management.jmxremote.port=43210"]}})
