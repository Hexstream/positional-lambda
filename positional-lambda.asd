(asdf:defsystem #:positional-lambda

  :author "Jean-Philippe Paradis <hexstream@gmail.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "positional-lambda is a concise, intuitive and flexible syntax (macro) for trivial lambdas that eschews explicit (and often contextually-redundant) naming of parameter variables in favor of positional references, with support for a used or ignored &rest parameter and automatic declaration of ignored parameters when logical \"gaps\" are left in the positional references. Also, its :let \"local special operator\" allows one to \"lift\" parts of its body outside the lambda to a LET without needing to name and then referer to an explicit variable."

  :depends-on (#:map-bind)

  :version "2.0"
  :serial cl:t
  :components ((:file "package")
	       (:file "main")))
