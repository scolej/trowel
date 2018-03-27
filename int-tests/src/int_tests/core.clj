(import 'java.lang.Runtime)

(ns int-tests.core
  (:gen-class)
  (:require [clj-http.client :as client])
  (:require [cheshire.core :as cheshire])
  (:require [clojure.java.io :as io]))

(def trowel-bin "../build/install/trowel/bin/trowel")
(def server "http://127.0.0.1:5555")
(def root (System/getProperty "user.dir"))

(defn abs-path [p]
  (.getPath (io/file root p)))

(defn exchange-json [j]
  (let [reply (client/post server {:content-type :json :body j})]
    (get reply :body)))

(defn test-response [request expected]
  (let [request-string (cheshire/generate-string request)
        reply-string (exchange-json request-string)
        reply-json (cheshire/parse-string reply-string true)]
    (if (= expected reply-json) (print "☻")
        (do (println "Assertion failed:\n"
                     "  Expected: " expected "\n"
                     "  Actual:   " reply-json)
            (throw (AssertionError.))))))

(defn build-default-lookup [step-text]
  {:rootDir "testing-ground"
   :action "lookup"
   :stepText step-text})

(defn run-tests []
  (let [p (. (Runtime/getRuntime) exec trowel-bin)
        r (clojure.java.io/reader (. p getInputStream))
        ;; Spawn a new guy to monitor what is going on.
        f (future (with-open [] (dorun (map println (line-seq r)))))]
    ;; Sleep to let the server start up. TODO Something better than this.
    (Thread/sleep 1000)
    (try
      (test-response (build-default-lookup "I have a thing.")
                     {:matches [{:file (abs-path "testing-ground/Glue.java")
                                 :lineNumber 2}
                                {:file (abs-path "testing-ground/Glue.java")
                                 :lineNumber 5}]})
      (test-response (build-default-lookup "I do a thing.")
                     {:matches [{:file (abs-path "testing-ground/Glue.java")
                                 :lineNumber 5}]})
      (test-response (build-default-lookup "I give a thing.")
                     {:matches [{:file (abs-path "testing-ground/Glue.java")
                                 :lineNumber 5}]})
      (test-response (build-default-lookup "(parens for trouble)")
                     {:matches [{:file (abs-path "testing-ground/Glue.java")
                                 :lineNumber 8}]})
      (test-response (build-default-lookup "\"quotes for trouble\"")
                     {:matches [{:file (abs-path "testing-ground/Glue.java")
                                 :lineNumber 11}]})
      (test-response (build-default-lookup "\\backslashes for trouble\\")
                     {:matches [{:file (abs-path "testing-ground/Glue.java")
                                 :lineNumber 14}]})
      (test-response (build-default-lookup "the dog is on the couch")
                     {:matches [{:file (abs-path "testing-ground/Glue.java")
                                 :lineNumber 17}]})
      (test-response (build-default-lookup "the cat is on the ROOF")
                     {:matches [{:file (abs-path "testing-ground/Glue.java")
                                 :lineNumber 20}]})
      (test-response (build-default-lookup "where are the penguins?")
                     {:matches []})
      (test-response (build-default-lookup "some sticky glue from another town...")
                     {:matches []})
      (test-response {:rootDir "proving-ground"
                      :action "lookup"
                      :stepText "some sticky glue from another town..."}
                     {:matches [{:file (abs-path "proving-ground/nest-for-trouble/Glue2.java")
                                 :lineNumber 2}]})
      ;; TODO Some error cases.
      ;; Example: rootDir not defined, missing action / steptext
      ;; Also FIXME stepText and lookup have mis-matched casing schemes.
      ;; (test-response {:action "lookup"
      ;;                 :stepText "anythung"}
      ;;                {:matches []})
      (finally
        (. p destroy)
        (shutdown-agents)))))

(defn -main
  [& args]
  (run-tests)
  (println "Done!")
  (System/exit 0))
