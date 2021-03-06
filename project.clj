(defproject raytracer "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.9.0"]]
  :target-path "target/%s"
  :global-vars {*unchecked-math* true
                *warn-on-reflection* true}
  :resource-paths ["resources"]
  :test-paths ["test" "test-slow"]
  :profiles {:demo-coverage {:main ^:skip-aot demo.all-demos-coverage}
             :quick-tests {:test-paths ^:replace ["test"]}
             :timed-run {:main ^:skip-aot demo.timed-run
                         :aot :all}
             :uberjar {:aot :all}             
             :visual-vm {:aot :all
                         :main demo.profile
                         :jvm-opts ["-Dcom.sun.management.jmxremote"
                                    "-Dcom.sun.management.jmxremote.ssl=false"
                                    "-Dcom.sun.management.jmxremote.authenticate=false"
                                    "-Dcom.sun.management.jmxremote.port=43210"]}})
