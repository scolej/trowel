# Trowel

[![Build Status](https://travis-ci.org/scolej/trowel.svg?branch=master)](https://travis-ci.org/scolej/trowel)

Trowel is a tiny service for looking up the Java glue code for
Cucumber steps. It scans a directory hierarchy containing Java glue
and reports the location of the glue defining a particular step.

Trowel listens on a port for HTTP POSTs containing JSON lookup requests,
and replies in JSON with the glue location.

An example interaction:

Client sends:

    {"action":"lookup","stepText":"the cat is on the ROOF"}

Trowel responds:

    {"matches":[{"file":"./testing-ground/Glue.java","lineNumber":7}]}

## To Do

- Caching. Trowel should not re-scan the whole hierarchy every time.
- Invalidate-cache request, to force a re-scan of the glue hierarchy.

- Handle keywords properly. Trowel should be able to identify and
  strip away the keyword from the step text. Currently the Elisp has
  absorbed this.

- Code coverage from Clojure tests.
- Use proper testing suite for the Clojure tests.
- Integrate the Gradle and Clojure builds so that it's not necessary
  to manually `gradle installDist` before running Clojure tests.

- Support requests which contain the whole `Scenario`, `Scenario
  Outline` or maybe even the whole file, to address GH-2. Could use
  the real Gherkin parser for this too.
- Need to be able to send a glue root argument so the same instance of
  Trowel can be used on different projects simultaneously.
