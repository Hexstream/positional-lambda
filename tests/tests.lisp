(cl:defpackage #:positional-lambda_tests
  (:use #:cl #:parachute)
  (:import-from #:positional-lambda #:plambda))

(cl:in-package #:positional-lambda_tests)

(defmacro are (comp expected form &optional description &rest format-args)
  `(is ,comp ,expected (multiple-value-list ,form) ,description ,@format-args))

(defmacro test-plambda (plambda input-args expected-argument-values)
  `(are equal ',expected-argument-values
        (funcall ,plambda ,@input-args)))

(define-test "featured-examples"
  (test-plambda (plambda (values :3 :1))
                (1 2 3)
                (3 1))
  (test-plambda (plambda (list :2 :1 :rest))
                (1 2 3 4)
                ((2 1 (3 4))))
  (test-plambda (plambda :rest (list :2 :1))
                (1 2 'ignored 'rest)
                ((2 1)))

  (test-plambda (plambda :3 :2)
                (1 2 3)
                (2))
  (test-plambda (plambda :3 (mapcar :1 :rest))
                (#'- 2 3 4 5)
                ((-4 -5)))

  (test-plambda (plambda :2)
                (1 2)
                (2))

  (with-open-stream (*standard-output* (make-string-output-stream))
    (test-plambda (plambda (princ :1)
                           (values (get-output-stream-string *standard-output*)
                                   :2))
                  (1 2)
                  ("1" 2)))

  ;;TODO: Test side-effects.
  (test-plambda (plambda :2 (list :1 (:let :random)))
                (1 2)
                ((1 :random)))

  (is string= "1111"
      (let ((fun (plambda (write :1 :base (:cache *print-base*)))))
        (with-output-to-string (*standard-output*)
          (let ((*print-base* 2))
            (funcall fun 3))
          (funcall fun 3))))

  ; TODO: (fail (plambda (:let (:let 1)))) ; undefined

  (test-plambda (plambda ':1)
                ()
                (:1))
  (test-plambda (plambda () (:quote :1))
                ()
                (:1)))
