# Trowel

![Master branch build status](https://travis-ci.org/scolej/trowel.svg?branch=master)

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
  strip away the keyword from the step text.
- An Elisp function for making requests and jumping to the identified
  location.
- Code coverage from Clojure tests.
