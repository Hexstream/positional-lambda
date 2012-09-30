(cl:defpackage #:positional-lambda_examples
  (:nicknames #:plambda_examples)
  (:use #:cl)
  (:import-from #:map-bind #:map-bind)
  (:import-from #:plambda #:plambda)
  (:export #:print-examples))

(in-package #:positional-lambda_examples)

(defparameter *this-directory*
  (make-pathname :name nil :type nil
                 :defaults #.(or *compile-file-truename* *load-truename*)))

(defparameter *examples* nil)

(defmacro define-examples (&body examples)
  `(setf *examples* ',examples))

(defparameter *pprint-dispatch*
  (let* ((table (copy-pprint-dispatch))
         (normal-print-keyword (pprint-dispatch :a-keyword)))
    (prog1 table
      (set-pprint-dispatch
       'keyword
       (lambda (stream keyword)
         (cond ((positional-lambda::%positional-ref-p keyword)
                (write-char #\: stream)
                (write-string (symbol-name keyword) stream))
               (t (funcall normal-print-keyword stream keyword))))
       0
       table))))

;;; I had forgotten how much of a bad joke trying to
;;; generate HTML without any kind of framework really is...
(defun print-examples (&key (format :html)
                       (file (make-pathname :name "examples" :type "html"
                                            :defaults *this-directory*)))
  (check-type format (member :html))
  (with-open-file (*standard-output* file
                                     :direction :output
                                     :if-exists :supersede)
    (format t "<p>This page is ugly and incomplete. Writing HTML and CSS by hand is too depressing. I'll make a great documentation system and update my old HTML and CSS libraries and then come back to this... Thank you for your patience!</p><table><thead><tr>~
                 <th>plambda</th>~
                 <th>Macroexpansion</th></tr>~
                 </thead><tbody>")
    (dolist (example *examples*)
      (destructuring-bind (nice) example
        (flet ((print-it (it)
                 (with-output-to-string (*standard-output*)
                   (write-string "<pre>")
                   (pprint it)
                   (write-string "</pre>"))))
          (format t "<tr><td>~A</td><td>~A</td></tr>"
                  (let ((*print-case* :downcase)
                        (*print-pprint-dispatch* *pprint-dispatch*))
                    (print-it nice))
                  (print-it (macroexpand-1 nice))))))
    (write-string "</tbody></table>")
    file))

(define-examples
  ((plambda (values :3 :1)))
  ((plambda (list* :2 :1 :rest)))
  ((plambda :rest (list :2 :1)))
  ((plambda :3 :2))
  ((plambda :3 (mapcar :1 :rest)))
  ((plambda :2))
  ((plambda (print :1) :2))
  ((plambda :2 (list :1 (:let (random)))))
  ((plambda (write :1 :base (:cache *print-base*)))))
