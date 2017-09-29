(import 'java.lang.Runtime)

(ns int-tests.core
  (:gen-class)
  (:require [clj-http.client :as client]))

(defn -main
  [& args]
  (try
    (def p (. (Runtime/getRuntime) exec "../build/install/trowel/bin/trowel"))
    (with-open [is (clojure.java.io/reader (. p getInputStream))]
      (dorun (map println (line-seq is))))
    ;; (client/post "http://127.0.0.1:5555"
    ;;              {:content-type :json
    ;;               :body "I have a thing."})
    (finally (. p destroy))))
