(import 'java.lang.Runtime)

(ns int-tests.core
  (:gen-class)
  (:require [clj-http.client :as client])
  (:require [cheshire.core :as cheshire]))

(def trowel-bin "../build/install/trowel/bin/trowel")
(def server "http://127.0.0.1:5555")

(defn exchange-json [j]
  (let [reply (client/post server {:content-type :json :body j})]
    (get reply :body)))

(defn test-response [request expected]
  (let [request-string (cheshire/generate-string request)
        reply-string (exchange-json request-string)
        reply-json (cheshire/parse-string reply-string true)]
    (when-not (= expected reply-json)
      (println "Assertion failed:\n"
               "  Expected: " expected "\n"
               "  Actual:   " reply-json)
      (throw (AssertionError.)))))

(defn -main
  [& args]
  (let [p (. (Runtime/getRuntime) exec trowel-bin)
        r (clojure.java.io/reader (. p getInputStream))
        ;; Spawn a new guy to monitor what is going on.
        f (future (with-open [] (dorun (map println (line-seq r)))))]
    ;; Sleep to let the server start up. TODO Something better than this.
    (Thread/sleep 1000)
    (try
      (test-response {:action "lookup"
                      :stepText "I have a thing."}
                     {:matches [{:file "./testing-ground/Glue.java"
                                 :lineNumber 1}
                                {:file "./testing-ground/Glue.java"
                                 :lineNumber 2}]})
      (test-response {:action "lookup"
                      :stepText "I do a thing."}
                     {:matches [{:file "./testing-ground/Glue.java"
                                 :lineNumber 2}]})
      (test-response {:action "lookup"
                      :stepText "I give a thing."}
                     {:matches [{:file "./testing-ground/Glue.java"
                                 :lineNumber 2}]})
      (test-response {:action "lookup"
                      :stepText "(parens for trouble)"}
                     {:matches [{:file "./testing-ground/Glue.java"
                                 :lineNumber 3}]})
      (test-response {:action "lookup"
                      :stepText "\"quotes for trouble\""}
                     {:matches [{:file "./testing-ground/Glue.java"
                                 :lineNumber 4}]})
      (test-response {:action "lookup"
                      :stepText "\\backslashes for trouble\\"}
                     {:matches [{:file "./testing-ground/Glue.java"
                                 :lineNumber 5}]})
      (test-response {:action "lookup"
                      :stepText "the dog is on the couch"}
                     {:matches [{:file "./testing-ground/Glue.java"
                                 :lineNumber 6}]})
      (test-response {:action "lookup"
                      :stepText "the cat is on the ROOF"}
                     {:matches [{:file "./testing-ground/Glue.java"
                                 :lineNumber 7}]})
      (test-response {:action "lookup"
                      :stepText "where are the penguins?"}
                     {:matches []})
      (finally (. p destroy)))))
