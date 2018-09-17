(asdf:defsystem #:positional-lambda_tests

  :author "Jean-Philippe Paradis <hexstream@gmail.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "POSITIONAL-LAMBDA unit tests."

  :depends-on ("positional-lambda"
               "parachute")

  :serial cl:t
  :components ((:file "tests"))

  :perform (asdf:test-op (op c) (uiop:symbol-call '#:parachute '#:test '#:positional-lambda_tests)))
