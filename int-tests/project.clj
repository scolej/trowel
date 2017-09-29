(defproject int-tests "0.1.0-SNAPSHOT"
  :description "Integration tests for Trowel."
  :url "none."
  :license {}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [clj-http "3.7.0"]]
  :main ^:skip-aot int-tests.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
